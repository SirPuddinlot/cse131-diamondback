// src/compiler.rs
use im::HashMap;
use crate::ast::*;
use crate::instr::*;

static mut LABEL_COUNTER: i32 = 0;
static mut HEAP_OFFSET: i32 = 0;  // Track heap allocations

fn new_label(prefix: &str) -> String {
    unsafe {
        LABEL_COUNTER += 1;
        format!("{}_{}", prefix, LABEL_COUNTER)
    }
}

// Allocate space on the heap for a define variable
fn alloc_heap_slot() -> i32 {
    unsafe {
        let offset = HEAP_OFFSET;
        HEAP_OFFSET += 8;  // 8 bytes per slot
        offset
    }
}

const TRUE_VAL: i32 = 3;   // 0b11
const FALSE_VAL: i32 = 1;  // 0b01

pub fn compile_to_instrs(
    e: &Expr, si: i32, 
    env: &HashMap<String, i32>, 
    defines: &HashMap<String, i32>,  // Now maps name -> heap_offset
    input: bool,
    loop_end: &Option<String> 
) -> Vec<Instr> {
    match e {
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))],
        Expr::Boolean(b) => {
            let val = if *b { TRUE_VAL } else { FALSE_VAL };
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(val))]
        }
        Expr::Input => {
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
        }
        Expr::Id(name) => {
            // Check stack environment first (for let-bound variables)
            if let Some(&offset) = env.get(name) {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))]
            }
            // Otherwise check if it's a defined variable (stored on heap)
            else if let Some(&heap_offset) = defines.get(name) {
                // Load from heap using R15 as base pointer
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::R15, heap_offset))]
            } 
            else {
                panic!("Unbound variable identifier {}", name);
            }
        }
        Expr::UnOp(op, expr) => {
            let mut code = compile_to_instrs(expr, si, env, defines, input, loop_end);
            match op {
                Op1::Add1 => {
                    code.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    code.push(Instr::IJne("error_invalid_argument".to_string()));
                    code.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
                }
                Op1::Sub1 => {
                    code.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    code.push(Instr::IJne("error_invalid_argument".to_string()));
                    code.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
                }
                Op1::IsNum => {
                    code.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    code.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(TRUE_VAL)));
                    code.push(Instr::ICMovE(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                }
                Op1::IsBool => {
                    code.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    code.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL)));
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(FALSE_VAL)));
                    code.push(Instr::ICMovE(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                }
            }
            code
        }
        Expr::BinOp(op, left, right) => {
            let mut code = compile_to_instrs(left, si, env, defines, input, loop_end);
            code.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            
            let right_code = compile_to_instrs(right, si - 8, env, defines, input, loop_end);
            code.extend(right_code);
            
            match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                    code.push(Instr::IOr(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RSP, si)));
                    code.push(Instr::ITest(Val::Reg(Reg::RCX), Val::Imm(1)));
                    code.push(Instr::IJne("error_invalid_argument".to_string()));
                    
                    match op {
                        Op2::Plus => {
                            code.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
                            code.push(Instr::IJo("error_overflow".to_string()));
                        }
                        Op2::Minus => {
                            code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                            code.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
                            code.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                            code.push(Instr::IJo("error_overflow".to_string()));
                        }
                        Op2::Times => {
                            code.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                            code.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
                            code.push(Instr::IJo("error_overflow".to_string()));
                        }
                        _ => unreachable!(),
                    }
                }
                Op2::Less | Op2::Greater | Op2::LessEqual | Op2::GreaterEqual => {
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                    code.push(Instr::IOr(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RSP, si)));
                    code.push(Instr::ITest(Val::Reg(Reg::RCX), Val::Imm(1)));
                    code.push(Instr::IJne("error_invalid_argument".to_string()));
                    
                    code.push(Instr::ICmp(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
                    code.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(TRUE_VAL)));
                    
                    match op {
                        Op2::Less => code.push(Instr::ICMovL(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX))),
                        Op2::Greater => code.push(Instr::ICMovG(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX))),
                        Op2::LessEqual => code.push(Instr::ICMovLE(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX))),
                        Op2::GreaterEqual => code.push(Instr::ICMovGE(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX))),
                        _ => unreachable!(),
                    }
                }
                Op2::Equal => {
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                    code.push(Instr::IXor(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RSP, si)));
                    code.push(Instr::ITest(Val::Reg(Reg::RCX), Val::Imm(1)));
                    code.push(Instr::IJne("error_invalid_argument".to_string()));
                    
                    code.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
                    code.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL)));
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(FALSE_VAL)));
                    code.push(Instr::ICMovNE(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                }
            }
            code
        }
        Expr::Set(name, expr) => {
            // Compile the value expression first
            let mut code = compile_to_instrs(expr, si, env, defines, input, loop_end);
        
            if let Some(offset) = env.get(name) {
                // Local variable on stack → store at stack offset
                code.push(Instr::IMov(Val::RegOffset(Reg::RSP, *offset), Val::Reg(Reg::RAX)));
            } 
            else if let Some(&heap_offset) = defines.get(name) {
                // Top-level define → store to heap using R15 as base
                code.push(Instr::IMov(Val::RegOffset(Reg::R15, heap_offset), Val::Reg(Reg::RAX)));
            } 
            else {
                panic!("Unbound variable identifier {}", name);
            }
        
            // set! evaluates to the value in RAX
            code
        }        
        Expr::If(cond, then_expr, else_expr) => {
            let else_label = new_label("else");
            let end_label = new_label("endif");
            
            let mut code = compile_to_instrs(cond, si, env, defines, input, loop_end);
            
            code.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
            code.push(Instr::IJe(else_label.clone()));
            
            let then_code = compile_to_instrs(then_expr, si, env, defines, input, loop_end);
            code.extend(then_code);
            code.push(Instr::IJmp(end_label.clone()));
            
            code.push(Instr::ILabel(else_label));
            let else_code = compile_to_instrs(else_expr, si, env, defines, input, loop_end);
            code.extend(else_code);
            
            code.push(Instr::ILabel(end_label));
            code
        }
        Expr::Block(exprs) => {
            let mut code = Vec::new();
            for expr in exprs {
                let expr_code = compile_to_instrs(expr, si, env, defines, input, loop_end);
                code.extend(expr_code);
            }
            code
        }
        Expr::Let(bindings, body) => {
            let mut code = Vec::new();
            let mut new_env = env.clone();
            let mut current_si = si;
            
            let mut seen_names = HashMap::new();
            for (name, _) in bindings {
                if seen_names.contains_key(name) {
                    panic!("Duplicate binding");
                }
                seen_names = seen_names.update(name.clone(), ());
            }
            
            for (name, expr) in bindings.iter() {
                let expr_code = compile_to_instrs(expr, current_si - 8, &new_env, defines, input, loop_end);
                code.extend(expr_code);
                
                code.push(Instr::IMov(Val::RegOffset(Reg::RSP, current_si), Val::Reg(Reg::RAX)));
                new_env = new_env.update(name.clone(), current_si);
                current_si -= 8;
            }
            
            let body_code = compile_to_instrs(body, current_si, &new_env, defines, input, loop_end);
            code.extend(body_code);
            
            code
        }
        Expr::Loop(body) => {
            let loop_start = new_label("loop_start");
            let loop_end_label = new_label("loop_end");
            
            let mut code = Vec::new();
            code.push(Instr::ILabel(loop_start.clone()));
            
            let body_code = compile_to_instrs(body, si, env, defines, input, &Some(loop_end_label.clone()));
            code.extend(body_code);
            
            code.push(Instr::IJmp(loop_start));
            code.push(Instr::ILabel(loop_end_label));
            
            code
        }
        
        Expr::Break(expr) => {
            if loop_end.is_none() {
                panic!("break");
            }
            
            let loop_end_label = loop_end.as_ref().unwrap();
            let mut code = compile_to_instrs(expr, si, env, defines, input, loop_end);
            code.push(Instr::IJmp(loop_end_label.clone()));
            
            code
        }
    }
}

pub fn compile(e: &Expr) -> String {
    let instrs = compile_to_instrs(e, -8, &HashMap::new(), &HashMap::new(), true, &None);
    let mut asm_code = String::new();
    
    for instr in instrs {
        asm_code.push_str(&instr_to_str(&instr));
        asm_code.push('\n');
    }
    asm_code.push_str("  ret\n");
    
    asm_code.push_str("\nerror_overflow:\n");
    asm_code.push_str("  mov rdi, 1\n");
    asm_code.push_str("  call snek_error\n");
    asm_code.push_str("  ret\n");
    asm_code.push_str("\nerror_invalid_argument:\n");
    asm_code.push_str("  mov rdi, 2\n");
    asm_code.push_str("  call snek_error\n");
    asm_code.push_str("  ret\n");
    asm_code
}

// For REPL: compile a define and return its heap offset
pub fn compile_define(name: &str, expr: &Expr, defines: &HashMap<String, i32>) -> (i32, Vec<Instr>) {
    let heap_offset = alloc_heap_slot();
    let mut code = compile_to_instrs(expr, -8, &HashMap::new(), defines, false, &None);
    // Store the result in the heap
    code.push(Instr::IMov(Val::RegOffset(Reg::R15, heap_offset), Val::Reg(Reg::RAX)));
    (heap_offset, code)
}