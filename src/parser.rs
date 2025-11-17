// src/parser.rs
use sexp::*;
use sexp::Atom::*;
use crate::ast::*;
use im::HashMap;

pub fn is_keyword(s: &str) -> bool {
    matches!(s, 
        "let" | "add1" | "sub1" | "isnum" | "isbool" | 
        "+" | "-" | "*" | "<" | ">" | ">=" | "<=" | "=" |
        "if" | "block" | "loop" | "break" | "set!" | 
        "true" | "false" | "input" | "define" | "fun" | "print"
    )
}

pub fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => {
            if vec.len() != 2 {
                panic!("Invalid binding");
            }
            let name = match &vec[0] {
                Sexp::Atom(S(s)) => {
                    if is_keyword(s) {
                        panic!("keyword");
                    }
                    s.clone()
                },
                _ => panic!("Invalid binding: expected identifier"),
            };
            let expr = parse_expr(&vec[1]);
            (name, expr)
        }
        _ => panic!("Invalid binding: expected list"),
    }
}

pub fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            let n_i32 = i32::try_from(*n).unwrap();
            Expr::Number(n_i32)
        }
        Sexp::Atom(F(_)) => {
            panic!("floats not supported yet :)")
        }
        Sexp::Atom(S(name)) => {
            // reserved words
            match name.as_str() {
                // "let" | "add1" | "sub1" | "+" | "-" | "*" | "define" => {
                //     panic!("used reserved word {}", name)
                // }
                "true" => Expr::Boolean(true),
                "false" => Expr::Boolean(false),
                "input" => Expr::Input,
                keyword if is_keyword(keyword) => {
                    panic!("keyword");
                }
                _ => Expr::Id(name.to_string()),
            }
        }
        Sexp::List(vec) => {
            if vec.is_empty() {
                panic!("empty expr");
            }
            
            match &vec[0] {
                Sexp::Atom(S(op)) => match op.as_str() {
                    "add1" | "sub1" => {
                        if vec.len() != 2 {
                            panic!("Invalid: {} takes exactly one argument", op);
                        }
                        let op_enum = match op.as_str() {
                            "add1" => Op1::Add1,
                            "sub1" => Op1::Sub1,
                            _ => unreachable!(),
                        };
                        Expr::UnOp(op_enum, Box::new(parse_expr(&vec[1])))
                    }
                    "isnum" => {
                        if vec.len() != 2 {
                            panic!("Invalid: isnum takes exactly one argument");
                        }
                        Expr::UnOp(Op1::IsNum, Box::new(parse_expr(&vec[1])))
                    }
                    "isbool" => {
                        if vec.len() != 2 {
                            panic!("Invalid: isbool takes exactly one argument");
                        }
                        Expr::UnOp(Op1::IsBool, Box::new(parse_expr(&vec[1])))
                    }
                    "+" | "-" | "*" => {
                        if vec.len() != 3 {
                            panic!("{} takes exactly two arguments", op);
                        }
                        let op_enum = match op.as_str() {
                            "+" => Op2::Plus,
                            "-" => Op2::Minus,
                            "*" => Op2::Times,
                            _ => unreachable!(),
                        };
                        Expr::BinOp(
                            op_enum,
                            Box::new(parse_expr(&vec[1])),
                            Box::new(parse_expr(&vec[2])),
                        )
                    }
                    "<" | ">" | ">=" | "<=" | "=" => {
                        if vec.len() != 3 {
                            panic!("Invalid: {} takes exactly two arguments", op);
                        }
                        let op_enum = match op.as_str() {
                            "<" => Op2::Less,
                            ">" => Op2::Greater,
                            ">=" => Op2::GreaterEqual,
                            "<=" => Op2::LessEqual,
                            "=" => Op2::Equal,
                            _ => unreachable!(),
                        };
                        Expr::BinOp(
                            op_enum,
                            Box::new(parse_expr(&vec[1])),
                            Box::new(parse_expr(&vec[2])),
                        )
                    }
                    "let" => {
                        if vec.len() != 3 {
                            panic!("let takes exactly two arguments");
                        }
                        let bindings_list = match &vec[1] {
                            Sexp::List(list) => list,
                            _ => panic!("let bindings must be a list"),
                        };
                        let mut bindings = Vec::new();
                        for binding_sexp in bindings_list {
                            bindings.push(parse_bind(binding_sexp));
                        }
                        if bindings.is_empty() {
                            panic!("let requires at least one binding");
                        }
                        Expr::Let(bindings, Box::new(parse_expr(&vec[2])))
                    }                   
                    "if" => { 
                            if vec.len() != 4 {
                                panic!("Invalid: if takes exactly three arguments");
                            }
                            Expr::If(
                                Box::new(parse_expr(&vec[1])),
                                Box::new(parse_expr(&vec[2])),
                                Box::new(parse_expr(&vec[3])),
                            )
                    }
                    "block" => {
                        if vec.len() < 2 {
                            panic!("Invalid: block requires at least one expression");
                        }
                        let mut exprs = Vec::new();
                        for expr_sexp in &vec[1..] {
                            exprs.push(parse_expr(expr_sexp));
                        }
                        Expr::Block(exprs)
                    }
                    "set!" => {
                        if vec.len() != 3 {
                            panic!("Invalid: set! requires exactly two arguments");
                        }
                        let name = match &vec[1] {
                            Sexp::Atom(S(s)) => {
                                if is_keyword(s) {
                                    panic!("keyword");
                                }
                                s.clone()
                            }
                            _ => panic!("Invalid: first argument to set! must be an identifier"),
                        };
                        let value_expr = parse_expr(&vec[2]);
                        Expr::Set(name, Box::new(value_expr))
                    }
                    "loop" => {
                        if vec.len() != 2 {
                            panic!("Invalid: loop requires exactly one argument");
                        }
                        Expr::Loop(Box::new(parse_expr(&vec[1])))
                    }
                    
                    "break" => {
                        if vec.len() != 2 {
                            panic!("Invalid: break requires exactly one argument");
                        }
                        Expr::Break(Box::new(parse_expr(&vec[1])))
                    }

                    "print" => {
                        if vec.len() != 2 {
                            panic!("Invalid: print takes exactly one argument");
                        }
                        Expr::UnOp(Op1::Print, Box::new(parse_expr(&vec[1])))
                    }
                    // At the end of parse_expr's Sexp::List match, before the final _ => panic!
                    _ => {
                        // Try to parse as function call
                        if let Sexp::Atom(S(name)) = &vec[0] {
                            if is_keyword(name) {
                                panic!("unknown operation {}", name);
                            }
                            // It's a function call
                            let mut args = Vec::new();
                            for arg in &vec[1..] {
                                args.push(parse_expr(arg));
                            }
                            return Expr::Call(name.clone(), args);
                        } 
                        else {
                            panic!("expected operation or function name");
                        }
                    }
                    // _ => panic!("unknown operation {}", op),
                },
                _ => panic!("expected operation"),
            }
        }
    }
}
pub fn parse_repl_entry(s: &Sexp, depth: usize) -> Result<ReplEntry, String> {
    match s {
        Sexp::List(vec) if !vec.is_empty() => {
            if let Sexp::Atom(S(op)) = &vec[0] {
                match op.as_str() {
                    "define" => {
                        if depth > 0 {
                            return Err("Invalid".to_string());
                        }
                        if vec.len() != 3 {
                            return Err("Invalid: define takes exactly two arguments".to_string());
                        }
                        let name = match &vec[1] {
                            Sexp::Atom(S(s)) => s.clone(),
                            _ => return Err("Invalid: define name must be identifier".to_string()),
                        };
                        let expr = parse_expr(&vec[2]);
                        return Ok(ReplEntry::Define(name, Box::new(expr)));
                    }
                    "fun" => {
                        if depth > 0 {
                            return Err("Invalid".to_string());
                        }
                        if vec.len() != 3 {
                            return Err("Invalid: fun takes exactly two arguments - parameter list and body".to_string());
                        }
                        
                        // Parse parameter list: (fun (name param1 param2 ...) body)
                        let params = match &vec[1] {
                            Sexp::List(param_list) => {
                                let mut parsed_params = Vec::new();
                                for param in param_list {
                                    match param {
                                        Sexp::Atom(S(name)) => {
                                            if is_keyword(name) {
                                                return Err(format!("Invalid parameter name: {}", name));
                                            }
                                            parsed_params.push(name.clone());
                                        }
                                        _ => return Err("Invalid: parameter must be identifier".to_string()),
                                    }
                                }
                                if parsed_params.is_empty() {
                                    return Err("Invalid: function must have at least a name".to_string());
                                }
                                parsed_params
                            }
                            _ => return Err("Invalid: function parameter list must be a list".to_string()),
                        };
                        
                        // First parameter is function name, rest are actual parameters
                        let name = params[0].clone();
                        let actual_params = params[1..].to_vec();
                        
                        // Parse body
                        let body = parse_expr(&vec[2]);
                        
                        return Ok(ReplEntry::Fun(name, actual_params, *Box::new(body)));
                    }
                    _ => {} // Not a special form, fall through to expression
                }
            }
        }
        _ => {}
    }
    
    // If we get here, it's a regular expression
    Ok(ReplEntry::Expr(parse_expr(s)))
}


// diamondback stuff

pub fn parse_program(s: &Sexp) -> Program {
    let list = match s {
        Sexp::List(vec) => vec,
        _ => panic!("Program must be a list of definitions and expression"),
    };
    
    let mut defns = Vec::new();
    let mut main_expr = None;
    
    for (i, item) in list.iter().enumerate() {
        if i == list.len() - 1 {
            // Last item is the main expression
            main_expr = Some(parse_expr(item));
        } else {
            // Everything else should be a function definition
            defns.push(parse_defn(item));
        }
    }
    
    if main_expr.is_none() {
        panic!("Program must have at least one expression");
    }
    
    Program {
        defns,
        main: main_expr.unwrap(),
    }
}

fn parse_defn(s: &Sexp) -> FunDefn {
    match s {
        Sexp::List(vec) => {
            if vec.len() != 3 {
                panic!("Invalid function definition");
            }
            
            // Check first element is "fun"
            match &vec[0] {
                Sexp::Atom(S(op)) if op == "fun" => {}
                _ => panic!("Expected 'fun'"),
            }
            
            // Parse (name param1 param2 ...)
            let (name, params) = match &vec[1] {
                Sexp::List(sig) => {
                    if sig.is_empty() {
                        panic!("Function signature cannot be empty");
                    }
                    let name = match &sig[0] {
                        Sexp::Atom(S(n)) => n.clone(),
                        _ => panic!("Function name must be identifier"),
                    };
                    
                    let mut params = Vec::new();
                    let mut seen = HashMap::new();
                    for param in &sig[1..] {
                        match param {
                            Sexp::Atom(S(p)) => {
                                if is_keyword(p) {
                                    panic!("keyword");
                                }
                                if seen.contains_key(p) {
                                    panic!("Duplicate binding");
                                }
                                seen = seen.update(p.clone(), ());
                                params.push(p.clone());
                            }
                            _ => panic!("Parameter must be identifier"),
                        }
                    }
                    (name, params)
                }
                _ => panic!("Invalid function signature"),
            };
            
            let body = parse_expr(&vec[2]);
            
            FunDefn {
                name,
                params,
                body: Box::new(body),
            }
        }
        _ => panic!("Function definition must be a list"),
    }
}