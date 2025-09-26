use std::{cmp::min, collections::HashMap};

use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Width {
    Byte,
    Word,
    Long,
    Quad,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Register {
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mnemonic {
    MOV,
    POP,
    PUSH,
    RET,
    SUB,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
    Mnemonic(Mnemonic, Width),
    Register(Register, Width),
    LabelDef(&'a str),
    Label(&'a str),
    Constant(i64),
    EOF,
}

fn r(p: &str) -> Regex {
    return Regex::new(p).unwrap();
}

pub fn lex<'a>(input: &'a str) -> (Vec<Token<'a>>, Vec<&'a str>, Vec<&'a str>) {
    use Mnemonic::*;
    use Register::*;
    use Width::*;

    let mnemonics = [
        (MOV, r(r"^mov[blqw]$")),
        (POP, r(r"^pop[qw]$")),
        (PUSH, r(r"^push[qw]$")),
        (RET, r(r"^ret[qw]$")),
        (SUB, r(r"^sub[blqw]$")),
    ];

    let regs = [
        (RAX, r(r"^%([er]?ax|al)$")),
        (RCX, r(r"^%([er]?cx|cl)$")),
        (RDX, r(r"^%([er]?dx|dl)$")),
        (RBX, r(r"^%([er]?bx|bl)$")),
        (RSP, r(r"^%([er]?sp|spl)$")),
        (RBP, r(r"^%([er]?bp|bpl)$")),
        (RSI, r(r"^%([er]?si|sil)$")),
        (RDI, r(r"^%([er]?di|dil)$")),
        (R8, r(r"^%r8[bdw]?$")),
        (R9, r(r"^%r9[bdw]?$")),
        (R10, r(r"^%r10[bdw]?$")),
        (R11, r(r"^%r11[bdw]?$")),
        (R12, r(r"^%r12[bdw]?$")),
        (R13, r(r"^%r13[bdw]?$")),
        (R14, r(r"^%r14[bdw]?$")),
        (R15, r(r"^%r15[bdw]?$")),
    ]; //ignore the existence of _h registers

    let delims = r(r"\t|\n|, ");
    let mut it = delims.split(input).filter(|&c| c != "");

    let label = r(r"^[a-zA-Z._][a-zA-Z0-9_]*(:)?$");
    let constant = r(r"^\$-?[0-9]+$");

    let mut globals: Vec<&str> = Vec::new();
    let mut sections: Vec<&str> = Vec::new();
    let mut tokens: Vec<Token> = Vec::new();

    while let Some(s) = it.next() {
        // dbg!(&s);
        if s == ".globl" {
            globals.push(label.find(it.next().unwrap()).expect("Expected valid symbol").as_str());
        } else if s == ".section" {
            //skip for now
            let args: Vec<&str> = it.next().unwrap().split(',').collect();
            sections.push(args[0]);
        } else if constant.is_match(s) {
            tokens.push(Token::Constant(s[1..].parse().unwrap()));
        } else if let Some((m, _)) = mnemonics.iter().find(|(_, r)| r.is_match(s)) {
            tokens.push(Token::Mnemonic(
                *m,
                match s.chars().last().unwrap() {
                    'b' => Byte,
                    'w' => Word,
                    'l' => Long,
                    'q' => Quad,
                    _ => unreachable!(),
                },
            ));
        } else if let Some((r, _)) = regs.iter().find(|(_, r)| r.is_match(s)) {
            tokens.push(Token::Register(
                *r,
                match (s.chars().nth(1).unwrap(), s.chars().last().unwrap()) {
                    (_, 'l' | 'b') => Byte,
                    (_, 'w') => Word,
                    ('e', _) => Long,
                    (_, 'd') => Long,
                    ('r', 'x' | 'p' | 'i') => Quad,
                    (_, 'x' | 'p' | 'i') => Word,
                    _ => Quad,
                },
            ));
        } else if let Some(c) = label.captures(s) {
            tokens.push(match c.get(1) {
                Some(_) => Token::LabelDef(&s[..s.len() - 1]),
                None => Token::Label(s),
            });
        }
    }

    tokens.push(Token::EOF);
    tokens.push(Token::EOF);

    return (tokens, globals, sections);
}

fn pref(bytecode: &mut Vec<u8>, r1: Register, r2: Register, w: Width){
    let (r1, r2) = (r1 as u8, r2 as u8);
    if w == Width::Word {
        bytecode.push(0x66);
    }
    let rex = (w == Width::Quad) as u8 * 0x48 | (match w {
        Width::Byte => r1 > 3 || r2 > 3,
        _ => r1 > 7 || r2 > 7,
    } as u8 * 0x40) | (r1 / 8 * 4 + r2 / 8);
    if rex != 0 {
        bytecode.push(rex);
    }
}

pub fn parse<'a>(mut tokens: &[Token<'a>]) -> (Vec<u8>, HashMap<&'a str, usize>) {
    use Mnemonic::*;
    use Register::*;
    use Width::*;
    // dbg!(&tokens);
    let mut bytecode: Vec<u8> = Vec::new();
    let mut labels: HashMap<&str, usize> = HashMap::new();

    while tokens[0] != Token::EOF {
        use Token::*;
        if let Mnemonic(MOV, w) = tokens[0] {
            if let Register(src, rw) = tokens[1] && rw == w {
                if let Register(dst, rw) = tokens[2] && rw == w {
                    pref(&mut bytecode, src, dst, w);
                    bytecode.push(if w == Byte { 0x88 } else { 0x89 });
                    bytecode.push(0xc0 + src as u8 % 8 * 8 + dst as u8 % 8);
                    tokens = &tokens[2..];
                }
            } else if let Constant(i) = tokens[1] {
                if let Register(dst, rw) = tokens[2] && rw == w {
                    pref(&mut bytecode, RAX, dst, w);
                    bytecode.push(if w == Byte { 0xb0 } else { 0xb8 } + dst as u8 % 8);
                    bytecode.extend(&i.to_le_bytes()[0..1 << (w as u8)]);
                    tokens = &tokens[2..];
                }
            }
        } else if let Mnemonic(POP, w) = tokens[0] {
            if let Register(dst, rw) = tokens[1] && rw == w {
                pref(&mut bytecode, RAX, dst, if w == Quad { Long } else { w });
                bytecode.push(0x58 + dst as u8 % 8);
                tokens = &tokens[1..];
            }
        } else if let Mnemonic(PUSH, w) = tokens[0] {
            if let Register(dst, rw) = tokens[1] && rw == w {
                pref(&mut bytecode, RAX, dst, if w == Quad { Long } else { w });
                bytecode.push(0x50 + dst as u8 % 8);
                tokens = &tokens[1..];
            }
        } else if let Mnemonic(RET, Quad) = tokens[0] {
            bytecode.push(0xc3); //safe?
        } else if let Mnemonic(SUB, w) = tokens[0] {
            if let Constant(i) = tokens[1] {
                if let Register(dst, rw) = tokens[2] && rw == w {
                    pref(&mut bytecode, RAX, dst, w);
                    bytecode.push(if w == Byte { 0x80 } else { 0x81 });
                    bytecode.push(0xe8 + dst as u8 % 8);
                    bytecode.extend(&i.to_le_bytes()[0..1 << min(w as u8, 2)]);
                    tokens = &tokens[2..];
                }
            }
        } else if let LabelDef(label) = tokens[0] {
            labels.try_insert(label, bytecode.len()).expect("Duplicate Label Definition");
        }
        tokens = &tokens[1..];
    }

    return (bytecode, labels);
}

pub fn shdr(elf: &mut Vec<u8>, sh_name: u32, sh_type: u32, sh_flags: u64, sh_addr: usize, sh_offset: usize, sh_size: usize, sh_link: u32, sh_info: u32, sh_addralign: u64, sh_entsize: u64) {
    elf.extend(sh_name.to_le_bytes());
    elf.extend(sh_type.to_le_bytes());
    elf.extend(sh_flags.to_le_bytes());
    elf.extend(sh_addr.to_le_bytes());
    elf.extend(sh_offset.to_le_bytes());
    elf.extend(sh_size.to_le_bytes());
    elf.extend(sh_link.to_le_bytes());
    elf.extend(sh_info.to_le_bytes());
    elf.extend(sh_addralign.to_le_bytes());
    elf.extend(sh_entsize.to_le_bytes());
}

pub fn assemble(file: &str) -> Vec<u8> {
    let (tokens, globals, sections) = lex(file);
    let (bytecode, labels) = parse(&tokens);
    
    let mut elf: Vec<u8> = Vec::new();
    elf.extend(b"\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00"); //e_ident
    elf.extend(b"\x01\x00"); //e_type
    elf.extend(b"\x3e\x00"); //e_machine
    elf.extend(b"\x01\x00\x00\x00"); //e_version
    elf.extend(b"\x00\x00\x00\x00\x00\x00\x00\x00"); //e_entry
    elf.extend(b"\x00\x00\x00\x00\x00\x00\x00\x00"); //e_phoff
    elf.extend([0; 8]); //e_shoff !
    elf.extend(b"\x00\x00\x00\x00"); //e_flags
    elf.extend(b"\x40\x00"); //e_ehsize
    elf.extend(b"\x00\x00"); //e_phentsize
    elf.extend(b"\x00\x00"); //e_phnum
    elf.extend(b"\x40\x00"); //e_shentsize
    elf.extend(((7 + sections.len()) as u16).to_le_bytes()); //e_shnum
    elf.extend(((6 + sections.len()) as u16).to_le_bytes()); //e_shstrndx
    
    elf.extend(bytecode.iter()); //.text
    //.data
    //.bss
    //.section directives

    elf.resize(elf.len() - elf.len() % 8 + 8, 0); //align 8
    
    let mut symtab = vec![0u8; 24];
    let mut strtab = vec![0u8];
    
    for s in globals {
        symtab.extend((strtab.len() as u32).to_le_bytes());
        symtab.push(0x10);
        symtab.push(0x00);
        symtab.extend(b"\x01\x00");
        symtab.extend(labels.get(s).unwrap().to_le_bytes());
        symtab.extend(b"\x00\x00\x00\x00\x00\x00\x00\x00");
        strtab.extend(s.as_bytes());
        strtab.push(0);
    }

    let symoff = elf.len();
    elf.extend(symtab.iter());
    elf.extend(strtab.iter());

    let mut idxs: Vec<u32> = vec![];
    let mut shstrtab = Vec::from(b"\0.symtab\0.strtab\0.shstrtab\0.text\0.data\0.bss\0"); //idk hardcode ts for now

    for s in sections {
        idxs.push(shstrtab.len() as u32);
        shstrtab.extend(s.as_bytes());
        shstrtab.push(0);
    }

    elf.extend(shstrtab.iter());
    
    elf.resize(elf.len() - elf.len() % 8 + 8, 0); //align 8
    let shoff = (elf.len() as u64).to_le_bytes();
    elf[40..48].copy_from_slice(&shoff);
    elf.extend([0u8; 64]); // NULL section

    shdr(&mut elf, 0x1b, 1, 6, 0, 64, bytecode.len(), 0, 0, 1, 0); //.text
    shdr(&mut elf, 0x21, 1, 3, 0, 64 + bytecode.len(), 0, 0, 0, 1, 0); //.data
    shdr(&mut elf, 0x27, 8, 3, 0, 64 + bytecode.len(), 0, 0, 0, 1, 0); //.bss

    for i in idxs {
        shdr(&mut elf, i, 1, 0, 0, 64 + bytecode.len(), 0, 0, 0, 1, 0);
    }

    shdr(&mut elf, 0x01, 2, 0, 0, symoff, symtab.len(), 6, 1, 8, 24); //.symtab
    shdr(&mut elf, 0x09, 3, 0, 0, symoff + symtab.len(), strtab.len(), 0, 0, 1, 0); //.strtab
    shdr(&mut elf, 0x11, 3, 0, 0, symoff + symtab.len() + strtab.len(), shstrtab.len(), 0, 0, 1, 0); //.shstrtab

    return elf;
}
