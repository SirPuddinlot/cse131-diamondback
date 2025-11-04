// src/instr.rs
#[derive(Debug, Clone, Copy)]
pub enum Reg {
    RAX,
    RSP,
    RCX,
    RDI,
    R15,
}

#[derive(Debug, Clone)]
pub enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
pub enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    // NEW - Comparison and logical operations
    ICmp(Val, Val),
    ITest(Val, Val),
    // NEW - Conditional moves and jumps
    ICMovE(Val, Val),   // Move if equal
    ICMovNE(Val, Val),  // Move if not equal
    ICMovG(Val, Val),   // Move if greater
    ICMovGE(Val, Val),  // Move if greater or equal
    ICMovL(Val, Val),   // Move if less
    ICMovLE(Val, Val),  // Move if less or equal
    // NEW - Labels and jumps
    ILabel(String),
    IJmp(String),
    IJe(String),   // Jump if equal
    IJne(String),  // Jump if not equal
    // NEW - Overflow checking
    IJo(String),   // Jump if overflow
    // NEW - Comments for debugging
    IComment(String),

    IOr(Val, Val),
    IXor(Val, Val),
    ISar(Val, Val),  // Shift arithmetic right
}



pub fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => match reg {
            Reg::RAX => "rax".to_string(),
            Reg::RSP => "rsp".to_string(),
            Reg::RCX => "rcx".to_string(),
            Reg::RDI => "rdi".to_string(),
            Reg::R15 => "r15".to_string(),
        },
        Val::Imm(n) => format!("{}", n),
        Val::RegOffset(reg, offset) => {
            let reg_str = match reg {
                Reg::RAX => "rax",
                Reg::RSP => "rsp",
                Reg::RCX => "rcx",
                Reg::RDI => "rdi",
                Reg::R15 => "r15",
            };
            format!("[{} - {}]", reg_str, offset)
        }
    }
}

pub fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dest, src) => format!("  mov {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::IAdd(dest, src) => format!("  add {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ISub(dest, src) => format!("  sub {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::IMul(dest, src) => format!("  imul {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICmp(dest, src) => format!("  cmp {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ITest(dest, src) => format!("  test {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICMovE(dest, src) => format!("  cmove {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICMovNE(dest, src) => format!("  cmovne {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICMovG(dest, src) => format!("  cmovg {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICMovGE(dest, src) => format!("  cmovge {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICMovL(dest, src) => format!("  cmovl {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICMovLE(dest, src) => format!("  cmovle {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ILabel(label) => format!("{}:", label),
        Instr::IJmp(label) => format!("  jmp {}", label),
        Instr::IJe(label) => format!("  je {}", label),
        Instr::IJne(label) => format!("  jne {}", label),
        Instr::IJo(label) => format!("  jo {}", label),
        Instr::IComment(comment) => format!("  ; {}", comment),
        Instr::IOr(dest, src) => format!("  or {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::IXor(dest, src) => format!("  xor {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ISar(dest, src) => format!("  sar {}, {}", val_to_str(dest), val_to_str(src)),

    }
}