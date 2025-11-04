// src/main.rs
mod ast;
mod instr;
mod parser;
mod compiler;
mod jit;
mod repl;
mod helpers;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use im::HashMap;
use dynasmrt::*;
use std::mem;

use crate::parser::parse_expr;
use crate::compiler::compile;
use crate::jit::compile_to_jit;
use crate::repl::run_repl;
use crate::helpers::*;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <-c|-e|-g|-i> <input.snek> [output.s]", args[0]);
        eprintln!("  -c: Compile to assembly file (requires output file)");
        eprintln!("  -e: Execute directly using JIT compilation");
        eprintln!("  -g: Do both - execute and generate assembly");
        eprintln!("  -i: Interactive REPL mode");
        std::process::exit(1);
    }

    let flag = &args[1];
    
    match flag.as_str() {
        "-i" => {
            // REPL mode
            //println!("REPL flag is before running: {}", REPL.load(Ordering::SeqCst));

            REPL.store(true, Ordering::SeqCst);
            // println!("REPL flag is after: {}", REPL.load(Ordering::SeqCst));

            return run_repl();
        }
        _ => {}
    }

    if args.len() < 3 {
        eprintln!("Usage: {} <-c|-e|-g> <input.cobra> [arg]", args[0]);
        eprintln!("  -c: Compile to assembly file");
        eprintln!("  -e: Execute directly using JIT compilation");
        eprintln!("  -g: Do both - execute and generate assembly");
        std::process::exit(1);
    }

    let flag = &args[1];
    let in_name = &args[2];
    
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let sexp = sexp::parse(&in_contents).map_err(|e| {
        std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Parse error: {}", e))
    })?;

    let expr = parse_expr(&sexp);
    
    match flag.as_str() {
        "-c" => {
            // AOT compilation only
            let out_name = &args[3];
            let result = compile(&expr);
            let asm_program = format!(
                "section .text\nglobal our_code_starts_here\nextern snek_error\nour_code_starts_here:\n{}",
                result
            );
            //println!("{}", asm_program);
            let mut out_file = File::create(out_name)?;
            out_file.write_all(asm_program.as_bytes())?;
        }
        "-e" => {
            // JIT compilation and execution only
            let input_str = if args.len() > 3 {
                &args[3]
            } else {
                "false"
            };
            
            let input = parse_input(input_str);
            
            eprintln!("Input string: {}", input_str);
            eprintln!("Input value (tagged): {:#018b} = {}", input, input);
            if input & 1 == 0 {
                eprintln!("  Represents number: {}", input >> 1);
            }
            
            let mut ops = dynasmrt::x64::Assembler::new().unwrap();
            
            let heap: Vec<i64> = vec![0; 128 * 1024]; // 1MB heap
            let heap_ptr = heap.as_ptr() as i64;
            

            let start = ops.offset();

            dynasm!(ops
                ; .arch x64
                ; mov r15, QWORD heap_ptr as _
            );
            
            compile_to_jit(&expr, &mut ops, &mut HashMap::new());

            let buf = ops.finalize().unwrap();
            let jitted_fn: extern "C" fn(i64) -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
            
            // eprintln!("\nCalling JIT function with input: {}", input);
            let result_val = jitted_fn(input);

            std::mem::forget(heap);
            
            print_result(result_val);
        }
        "-g" => {
            // Both: JIT execution and AOT compilation
            let input = if args.len() > 3 {
                parse_input(&args[3])
            } else {
                FALSE_VAL
            };
            
            // JIT compilation and execution
            let mut ops = dynasmrt::x64::Assembler::new().unwrap();
            let start = ops.offset();
            
            compile_to_jit(&expr, &mut ops, &mut HashMap::new());
            
            // dynasm!(ops
            //     ; .arch x64
            //     ; ret
            // );

            let buf = ops.finalize().unwrap();
            let jitted_fn: extern "C" fn(i64) -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
            let result_val = jitted_fn(input);
            
            println!("JIT Result: ");
            print_result(result_val);
            
            // AOT compilation output
            println!("\n=== Generated Assembly ===");
            let result = compile(&expr);
            let asm_program = format!(
                "section .text\nglobal our_code_starts_here\nextern snek_error\nour_code_starts_here:\n{}",
                result
            );
            println!("{}", asm_program);
        }
        _ => {
            eprintln!("Error: Unknown flag '{}'", flag);
            eprintln!("Usage: {} <-c|-e|-g> <input.cobra> [arg]", args[0]);
            std::process::exit(1);
        }
    }

    Ok(())
}