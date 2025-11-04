// src/jit.rs
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};
use dynasmrt::x64::Assembler;
use im::HashMap;
use crate::ast::*;
use crate::instr::*;
use crate::compiler::compile_to_instrs;
use std::collections::HashMap as StdHashMap;

pub fn compile_to_jit(e: &Expr, ops: &mut Assembler, defines: &mut HashMap<String, i32>) {
    let instrs = compile_to_instrs(e, -8, &HashMap::new(), defines, true, &None);

    // DEBUG: Print all instructions that will be JIT compiled
    eprintln!("=== JIT Instructions ===");
    for instr in &instrs {
        eprintln!("{}", crate::instr::instr_to_str(instr));
    }
    eprintln!("========================\n");
    
    // Create a map of label names to dynamic labels
    let mut label_map: StdHashMap<String, dynasmrt::DynamicLabel> = StdHashMap::new();
    
    // Pre-create error handler labels
    let error_overflow = ops.new_dynamic_label();
    let error_invalid_arg = ops.new_dynamic_label();
    label_map.insert("error_overflow".to_string(), error_overflow);
    label_map.insert("error_invalid_argument".to_string(), error_invalid_arg);
    
    // First pass: collect all other labels
    for instr in &instrs {
        if let Instr::ILabel(label_name) = instr {
            if !label_map.contains_key(label_name) {
                label_map.insert(label_name.clone(), ops.new_dynamic_label());
            }
        }
        // Also collect jump targets
        match instr {
            Instr::IJmp(label) | Instr::IJe(label) | Instr::IJne(label) | Instr::IJo(label) => {
                if !label_map.contains_key(label) {
                    label_map.insert(label.clone(), ops.new_dynamic_label());
                }
            }
            _ => {}
        }
    }
    
    // Second pass: emit instructions
    for instr in &instrs {
        instr_to_dynasm(instr, ops, &label_map);
    }

    dynasm!(ops
        ; .arch x64
        ; ret
    );

    let snek_error_addr = crate::snek_error as *const () as i64;
    // Add error handlers at the end
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
}

pub fn instr_to_dynasm(instr: &Instr, ops: &mut Assembler, label_map: &StdHashMap<String, dynasmrt::DynamicLabel>) {
    match instr {
        Instr::IMov(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; mov rax, QWORD *n as i64
                    );
                }
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; mov rax, rcx
                    );
                }
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; mov rax, rdi
                    );
                }
                (Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; mov rcx, rax
                    );
                }
                (Val::Reg(Reg::RCX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; mov rcx, QWORD *n as i64
                    );
                }
                (Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; mov rax, [rsp - pos_offset]
                    );
                }
                (Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; mov [rsp - pos_offset], rax
                    );
                }
                // Load from heap: mov rax, [r15 + offset]
                (Val::Reg(Reg::RAX), Val::RegOffset(Reg::R15, offset)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; mov rax, [r15 + *offset]
                    );
                }
                
                // Store to heap: mov [r15 + offset], rax
                (Val::RegOffset(Reg::R15, offset), Val::Reg(Reg::RAX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; mov [r15 + *offset], rax
                    );
                }
                
                _ => panic!("Unsupported mov pattern in JIT: {:?} <- {:?}", dest, src),
            }
        }
        Instr::IAdd(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; add rax, *n as i32
                    );
                }
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; add rax, rcx
                    );
                }
                (Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; add rax, [rsp - pos_offset]
                    );
                }
                _ => panic!("Unsupported add pattern in JIT: {:?} += {:?}", dest, src),
            }
        }
        Instr::ISub(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; sub rax, *n as i32
                    );
                }
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; sub rax, rcx
                    );
                }
                (Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; sub rax, [rsp - pos_offset]
                    );
                }
                _ => panic!("Unsupported sub pattern in JIT: {:?} -= {:?}", dest, src),
            }
        }
        Instr::IMul(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; imul rax, rax, *n as i32
                    );
                }
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; imul rax, rcx
                    );
                }
                (Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; imul rax, [rsp - pos_offset]
                    );
                }
                _ => panic!("Unsupported imul pattern in JIT: {:?} *= {:?}", dest, src),
            }
        }
        Instr::ICmp(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; cmp rax,  *n as i32
                    );
                }
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; cmp rax, rcx
                    );
                }
                (Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; cmp QWORD [rsp - pos_offset], rax
                    );
                }
                (Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; cmp rcx, rax
                    );
                }
                (Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; cmp rax, [rsp - pos_offset]
                    );
                }
                _ => panic!("Unsupported cmp pattern in JIT: {:?} cmp {:?}", dest, src),
            }
        }
        Instr::ITest(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; test rax, *n as i32
                    );
                }
                (Val::Reg(Reg::RCX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; test rcx, *n as i32
                    );
                }
                _ => panic!("Unsupported test pattern in JIT: {:?} test {:?}", dest, src),
            }
        }
        Instr::ICMovE(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; cmove rax, rcx
                    );
                }
                _ => panic!("Unsupported cmove pattern in JIT: {:?} <- {:?}", dest, src),
            }
        }
        Instr::ICMovNE(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; cmovne rax, rcx
                    );
                }
                _ => panic!("Unsupported cmovne pattern in JIT: {:?} <- {:?}", dest, src),
            }
        }
        Instr::ICMovG(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; cmovg rax, rcx
                    );
                }
                _ => panic!("Unsupported cmovg pattern in JIT: {:?} <- {:?}", dest, src),
            }
        }
        Instr::ICMovGE(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; cmovge rax, rcx
                    );
                }
                _ => panic!("Unsupported cmovge pattern in JIT: {:?} <- {:?}", dest, src),
            }
        }
        Instr::ICMovL(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; cmovl rax, rcx
                    );
                }
                _ => panic!("Unsupported cmovl pattern in JIT: {:?} <- {:?}", dest, src),
            }
        }
        Instr::ICMovLE(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; cmovle rax, rcx
                    );
                }
                _ => panic!("Unsupported cmovle pattern in JIT: {:?} <- {:?}", dest, src),
            }
        }
        Instr::ILabel(label_name) => {
            if let Some(&label) = label_map.get(label_name) {
                dynasm!(ops
                    ; .arch x64
                    ; =>label
                );
            }
        }
        Instr::IJmp(label_name) => {
            if let Some(&label) = label_map.get(label_name) {
                dynasm!(ops
                    ; .arch x64
                    ; jmp =>label
                );
            }
        }
        Instr::IJe(label_name) => {
            if let Some(&label) = label_map.get(label_name) {
                dynasm!(ops
                    ; .arch x64
                    ; je =>label
                );
            }
        }
        Instr::IJne(label_name) => {
            if let Some(&label) = label_map.get(label_name) {
                dynasm!(ops
                    ; .arch x64
                    ; jne =>label
                );
            }
        }
        Instr::IJo(label_name) => {
            if let Some(&label) = label_map.get(label_name) {
                dynasm!(ops
                    ; .arch x64
                    ; jo =>label
                );
            }
        }
        Instr::IOr(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RCX), Val::RegOffset(Reg::RSP, offset)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; or rcx, [rsp - pos_offset]
                    );
                }
                _ => panic!("Unsupported or pattern in JIT: {:?} | {:?}", dest, src),
            }
        }
        Instr::IXor(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RCX), Val::RegOffset(Reg::RSP, offset)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; xor rcx, [rsp - pos_offset]
                    );
                }
                _ => panic!("Unsupported xor pattern in JIT: {:?} ^ {:?}", dest, src),
            }
        }
        Instr::ISar(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; sar rax, *n as i8
                    );
                }
                _ => panic!("Unsupported sar pattern in JIT: {:?} >> {:?}", dest, src),
            }
        }
        Instr::IComment(_) => {
            // Comments are ignored in JIT
        }
    }
}