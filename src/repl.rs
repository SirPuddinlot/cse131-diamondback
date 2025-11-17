// src/repl.rs
use std::io::{self, Write, BufRead};
use im::HashMap;
use std::mem;
use dynasmrt::*;
use std::collections::HashMap as StdHashMap;
use crate::ast::*;
use crate::parser::*;
use crate::jit::*;
use crate::compiler::{FunContext, get_input_heap_offset};

fn print_result(val: i64) {
    if val & 1 == 0 {
        // It's a number
        println!("{}", val >> 1);
    } else if val == 1 {
        println!("true");
    } else if val == 3 {
        println!("false");
    } else {
        println!("Unknown value: {}", val);
    }
}

pub fn run_repl() -> io::Result<()> {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let mut defines: HashMap<String, i32> = HashMap::new();
    let mut functions: Vec<FunDefn> = Vec::new();
    let mut label_map: StdHashMap<String, dynasmrt::DynamicLabel> = StdHashMap::new();
    
    // Allocate heap once at the start
    let heap: Vec<i64> = vec![0; 128 * 1024];
    let heap_ptr = heap.as_ptr() as i64;
    
    // Pre-create error handler labels
    let snek_print = ops.new_dynamic_label();
    let error_overflow = ops.new_dynamic_label();
    let error_invalid_arg = ops.new_dynamic_label();
    label_map.insert("_snek_print".to_string(), snek_print);
    label_map.insert("error_overflow".to_string(), error_overflow);
    label_map.insert("error_invalid_argument".to_string(), error_invalid_arg);
    
    // Compile error handlers once at the start
    let snek_error_addr = crate::helpers::snek_error as *const () as i64;
    let snek_print_addr = crate::helpers::_snek_print as *const () as i64;

    dynasm!(ops
        ; .arch x64
        ; =>snek_print
        ; push rbp
        ; mov rbp, rsp
        ; mov rax, QWORD snek_print_addr as _
        ; call rax
        ; pop rbp
        ; ret
    );
    
    dynasm!(ops
        ; .arch x64
        ; =>error_overflow
        ; mov rdi, 1
        ; mov rax, QWORD snek_error_addr as _
        ; call rax
        ; ret
        ; =>error_invalid_arg
        ; mov rdi, 2
        ; mov rax, QWORD snek_error_addr as _
        ; call rax
        ; ret
    );
    
    ops.commit().unwrap();
    
    let stdin = io::stdin();
    let mut reader = stdin.lock();
    
    loop {
        print!("> ");
        io::stdout().flush()?;
        
        let mut input = String::new();
        reader.read_line(&mut input)?;
        
        let input = input.trim();
        
        // Check for exit commands
        if input == "exit" || input == "quit" {
            break;
        }
        
        if input.is_empty() {
            continue;
        }
        
        // Parse the input
        let sexp = match sexp::parse(input) {
            Ok(s) => s,
            Err(_e) => {
                println!("Invalid: parse error");
                continue;
            }
        };
        
        // Parse into ReplEntry - catch panics from parser
        let entry = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            parse_repl_entry(&sexp, 0)
        })) {
            Ok(Ok(e)) => e,
            Ok(Err(msg)) => {
                println!("{}", msg);
                continue;
            }
            Err(_) => {
                println!("Invalid");
                continue;
            }
        };
        
        match entry {
            ReplEntry::FunDefn(defn) => {
                // Check for duplicate function definition
                if functions.iter().any(|f| f.name == defn.name) {
                    println!("Duplicate function definition: {}", defn.name);
                    continue;
                }
                
                // Check that function body doesn't use input
                if contains_input(&defn.body) {
                    println!("Invalid: input not allowed in function definitions");
                    continue;
                }
                
                functions.push(defn.clone());
                
                // Rebuild the function context
                let fun_ctx = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    FunContext::new(&functions)
                })) {
                    Ok(ctx) => ctx,
                    Err(_) => {
                        println!("Invalid");
                        functions.pop(); // Remove the function we just added
                        continue;
                    }
                };
                
                // Pre-create label for this function
                let fun_label = ops.new_dynamic_label();
                label_map.insert(format!("fun_{}", defn.name), fun_label);
                
                // Compile function prologue
                dynasm!(ops
                    ; .arch x64
                    ; =>fun_label
                    ; push rbp
                    ; mov rbp, rsp
                );
                
                // Build environment: parameters are on caller's stack at [rbp+16], [rbp+24], etc.
                let mut env = HashMap::new();
                for (i, param) in defn.params.iter().enumerate() {
                    let offset = 16 + (i as i32 * 8);
                    env = env.update(param.clone(), offset);
                }
                
                // Compile function body
                let (instrs, min_offset) = match std::panic::catch_unwind(
                    std::panic::AssertUnwindSafe(|| {
                        crate::compiler::compile_to_instrs(
                            &defn.body,
                            -8,
                            &env,
                            &defines,
                            &fun_ctx,
                            false,
                            &None,
                        )
                    })
                ) {
                    Ok(result) => result,
                    Err(_) => {
                        println!("Invalid");
                        functions.pop();
                        label_map.remove(&format!("fun_{}", defn.name));
                        continue;
                    }
                };
                
                // Allocate stack space if needed
                if min_offset < 0 {
                    let needed = -min_offset;
                    let stack_space = ((needed + 15) / 16) * 16;
                    dynasm!(ops
                        ; .arch x64
                        ; sub rsp, stack_space as i32
                    );
                }
                
                // Pre-create labels for function body
                for instr in &instrs {
                    if let crate::instr::Instr::ILabel(label_name) = instr {
                        if !label_map.contains_key(label_name) {
                            label_map.insert(label_name.clone(), ops.new_dynamic_label());
                        }
                    }
                    match instr {
                        crate::instr::Instr::IJmp(l) | crate::instr::Instr::IJe(l) | 
                        crate::instr::Instr::IJne(l) | crate::instr::Instr::IJo(l) => {
                            if !label_map.contains_key(l) {
                                label_map.insert(l.clone(), ops.new_dynamic_label());
                            }
                        }
                        _ => {}
                    }
                }
                
                // Emit function body instructions
                for instr in &instrs {
                    instr_to_dynasm(instr, &mut ops, &label_map);
                }
                
                // Function epilogue
                dynasm!(ops
                    ; .arch x64
                    ; mov rsp, rbp
                    ; pop rbp
                    ; ret
                );
                
                match ops.commit() {
                    Ok(_) => {
                        println!("Function defined: {}", defn.name);
                    }
                    Err(_) => {
                        println!("Invalid");
                        functions.pop();
                        label_map.remove(&format!("fun_{}", defn.name));
                        continue;
                    }
                }
            }
            ReplEntry::Fun(name, params, body) => {
                // Convert to FunDefn and process
                let defn = FunDefn {
                    name: name.clone(),
                    params: params.clone(),
                    body: Box::new(body),
                };
                
                // Check for duplicate function definition
                if functions.iter().any(|f| f.name == name) {
                    println!("Duplicate function definition: {}", name);
                    continue;
                }
                
                // Check that function body doesn't use input
                if contains_input(&defn.body) {
                    println!("Invalid: input not allowed in function definitions");
                    continue;
                }
                
                functions.push(defn.clone());
                
                // Same compilation logic as FunDefn case above
                let fun_ctx = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    FunContext::new(&functions)
                })) {
                    Ok(ctx) => ctx,
                    Err(_) => {
                        println!("Invalid");
                        functions.pop();
                        continue;
                    }
                };
                
                let fun_label = ops.new_dynamic_label();
                label_map.insert(format!("fun_{}", name), fun_label);
                
                dynasm!(ops
                    ; .arch x64
                    ; =>fun_label
                    ; push rbp
                    ; mov rbp, rsp
                );
                
                let mut env = HashMap::new();
                for (i, param) in params.iter().enumerate() {
                    let offset = 16 + (i as i32 * 8);
                    env = env.update(param.clone(), offset);
                }
                
                let (instrs, min_offset) = match std::panic::catch_unwind(
                    std::panic::AssertUnwindSafe(|| {
                        crate::compiler::compile_to_instrs(
                            &defn.body,
                            -8,
                            &env,
                            &defines,
                            &fun_ctx,
                            false,
                            &None,
                        )
                    })
                ) {
                    Ok(result) => result,
                    Err(_) => {
                        println!("Invalid");
                        functions.pop();
                        label_map.remove(&format!("fun_{}", name));
                        continue;
                    }
                };
                
                if min_offset < 0 {
                    let needed = -min_offset;
                    let stack_space = ((needed + 15) / 16) * 16;
                    dynasm!(ops
                        ; .arch x64
                        ; sub rsp, stack_space as i32
                    );
                }
                
                for instr in &instrs {
                    if let crate::instr::Instr::ILabel(label_name) = instr {
                        if !label_map.contains_key(label_name) {
                            label_map.insert(label_name.clone(), ops.new_dynamic_label());
                        }
                    }
                    match instr {
                        crate::instr::Instr::IJmp(l) | crate::instr::Instr::IJe(l) | 
                        crate::instr::Instr::IJne(l) | crate::instr::Instr::IJo(l) => {
                            if !label_map.contains_key(l) {
                                label_map.insert(l.clone(), ops.new_dynamic_label());
                            }
                        }
                        _ => {}
                    }
                }
                
                for instr in &instrs {
                    instr_to_dynasm(instr, &mut ops, &label_map);
                }
                
                dynasm!(ops
                    ; .arch x64
                    ; mov rsp, rbp
                    ; pop rbp
                    ; ret
                );
                
                match ops.commit() {
                    Ok(_) => {
                        println!("Function defined: {}", name);
                    }
                    Err(_) => {
                        println!("Invalid");
                        functions.pop();
                        label_map.remove(&format!("fun_{}", name));
                        continue;
                    }
                }
            }
            ReplEntry::Define(name, expr) => {
                // Check for duplicate definition
                if defines.contains_key(&name) {
                    println!("Duplicate binding");
                    continue;
                }
                
                // Build function context
                let fun_ctx = FunContext::new(&functions);
                
                // Compile the define
                let (heap_offset, instrs) = match std::panic::catch_unwind(
                    std::panic::AssertUnwindSafe(|| {
                        crate::compiler::compile_define(&name, &expr, &defines, &fun_ctx)
                    })
                ) {
                    Ok(result) => result,
                    Err(_) => {
                        println!("Invalid");
                        continue;
                    }
                };
                
                let start = ops.offset();
                
                // Set up heap pointer
                dynasm!(ops
                    ; .arch x64
                    ; push rbp
                    ; mov rbp, rsp
                    ; mov r15, QWORD heap_ptr as _
                );
                
                // Pre-create any labels needed
                for instr in &instrs {
                    if let crate::instr::Instr::ILabel(label_name) = instr {
                        if !label_map.contains_key(label_name) {
                            label_map.insert(label_name.clone(), ops.new_dynamic_label());
                        }
                    }
                    match instr {
                        crate::instr::Instr::IJmp(l) | crate::instr::Instr::IJe(l) | 
                        crate::instr::Instr::IJne(l) | crate::instr::Instr::IJo(l) => {
                            if !label_map.contains_key(l) {
                                label_map.insert(l.clone(), ops.new_dynamic_label());
                            }
                        }
                        _ => {}
                    }
                }
                
                // Emit the instructions
                for instr in &instrs {
                    instr_to_dynasm(instr, &mut ops, &label_map);
                }
                
                dynasm!(ops 
                    ; .arch x64 
                    ; pop rbp
                    ; ret
                );
                
                match ops.commit() {
                    Ok(_) => {}
                    Err(_) => {
                        println!("Invalid");
                        continue;
                    }
                }
                
                let reader = ops.reader();
                let buf = reader.lock();
                let jitted_fn: extern "C" fn() -> i32 = unsafe { mem::transmute(buf.ptr(start)) };
                jitted_fn(); // Execute to store the value
                
                // Store the heap offset
                defines = defines.update(name.clone(), heap_offset);
                println!("{} defined", name);
            }
            ReplEntry::Expr(expr) => {
                // Build function context
                let fun_ctx = FunContext::new(&functions);
                
                // Create a program with the expression as main
                let program = Program {
                    defns: functions.clone(),
                    main: expr,
                };
                
                let start = ops.offset();
                
                // Set up function prologue and heap pointer
                dynasm!(ops
                    ; .arch x64
                    ; push rbp
                    ; mov rbp, rsp
                    ; mov r15, QWORD heap_ptr as _
                );
                
                // Store input value (always false/3 in REPL)
                let input_heap_offset = get_input_heap_offset();
                dynasm!(ops
                    ; .arch x64
                    ; mov QWORD [r15 + input_heap_offset], 3  // false
                );
                
                // Compile the expression
                let (instrs, min_offset) = match std::panic::catch_unwind(
                    std::panic::AssertUnwindSafe(|| {
                        crate::compiler::compile_to_instrs(
                            &program.main,
                            -8,
                            &HashMap::new(),
                            &defines,
                            &fun_ctx,
                            true,
                            &None,
                        )
                    })
                ) {
                    Ok(result) => result,
                    Err(_) => {
                        println!("Invalid");
                        continue;
                    }
                };
                
                // Allocate stack space if needed
                if min_offset <= -16 {
                    let needed = -min_offset - 8;
                    let stack_space = ((needed + 15) / 16) * 16;
                    dynasm!(ops
                        ; .arch x64
                        ; sub rsp, stack_space as i32
                    );
                }
                
                // Pre-create labels
                for instr in &instrs {
                    if let crate::instr::Instr::ILabel(label_name) = instr {
                        if !label_map.contains_key(label_name) {
                            label_map.insert(label_name.clone(), ops.new_dynamic_label());
                        }
                    }
                    match instr {
                        crate::instr::Instr::IJmp(l) | crate::instr::Instr::IJe(l) | 
                        crate::instr::Instr::IJne(l) | crate::instr::Instr::IJo(l) => {
                            if !label_map.contains_key(l) {
                                label_map.insert(l.clone(), ops.new_dynamic_label());
                            }
                        }
                        _ => {}
                    }
                }
                
                // Emit instructions
                for instr in &instrs {
                    instr_to_dynasm(instr, &mut ops, &label_map);
                }
                
                dynasm!(ops 
                    ; .arch x64 
                    ; mov rsp, rbp
                    ; pop rbp
                    ; ret
                );
                
                match ops.commit() {
                    Ok(_) => {}
                    Err(_) => {
                        println!("Invalid");
                        continue;
                    }
                }
                
                let reader = ops.reader();
                let buf = reader.lock();
                let jitted_fn: extern "C" fn() -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
                let result = jitted_fn();
                
                print_result(result);
            }
        }
    }
    
    std::mem::forget(heap); // Don't drop the heap
    Ok(())
}

// Helper function to check if an expression contains input
fn contains_input(expr: &Expr) -> bool {
    match expr {
        Expr::Input => true,
        Expr::UnOp(_, e) => contains_input(e),
        Expr::BinOp(_, e1, e2) => contains_input(e1) || contains_input(e2),
        Expr::If(e1, e2, e3) => contains_input(e1) || contains_input(e2) || contains_input(e3),
        Expr::Let(bindings, body) => {
            bindings.iter().any(|(_, e)| contains_input(e)) || contains_input(body)
        }
        Expr::Block(exprs) => exprs.iter().any(|e| contains_input(e)),
        Expr::Set(_, e) => contains_input(e),
        Expr::Loop(e) => contains_input(e),
        Expr::Break(e) => contains_input(e),
        Expr::Call(_, args) => args.iter().any(|e| contains_input(e)),
        _ => false,
    }
}