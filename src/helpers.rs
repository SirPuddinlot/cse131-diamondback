// helpers
use std::sync::atomic::AtomicBool;
use crate::Ordering;
pub const TRUE_VAL: i64 = 1;
pub const FALSE_VAL: i64 = 3;

pub static REPL: AtomicBool = AtomicBool::new(false);

pub fn parse_input(input: &str) -> i64 {
    match input {
        "true" => TRUE_VAL,
        "false" => FALSE_VAL,
        _ => {
            let num = input.parse::<i64>().unwrap_or_else(|_| {
                eprintln!("Invalid input: {}", input);
                std::process::exit(1);
            });
            
            if num < -4611686018427387904 || num > 4611686018427387903 {
                eprintln!("Input number out of range");
                std::process::exit(1);
            }
            num << 1  
        }
    }
}

pub fn print_result(val: i64) {
    if val & 1 == 1 {
        if val == TRUE_VAL {
            println!("true");
        } 
        else if val == FALSE_VAL {
            println!("false");
        } 
        // else if REPL.load(Ordering::SeqCst) {
        //     // println!("{}", REPL.load(Ordering::SeqCst));
        //     eprintln!("Invalid boolean value: {}", val);
        // }
        else {
            eprintln!("Invalid boolean value: {}", val);
            std::process::exit(1);
        }
    } else {
        println!("{}", val >> 1);
    }
}

// Export snek_error for JIT to call
#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    if errcode == 1 {
        eprintln!("overflow");
    } else if errcode == 2 {
        eprintln!("invalid argument");
    } else {
        eprintln!("error code: {}", errcode);
    }
    if !REPL.load(Ordering::SeqCst) {
        std::process::exit(1);
    }
}
