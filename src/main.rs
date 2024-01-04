use std::io::Write;

mod examples;

const SYSCALL_OFFSET: usize = 100;

#[macro_export]
macro_rules! CString {
    ($str:expr) => {
        CString(String::from($str))
    };
}

macro_rules! Raw {
    ($str:expr) => {
        Raw(String::from($str))
    };
}

#[macro_export]
macro_rules! syscall_seq {
    ($syscall_id:expr, [$($arg:expr),*]) => {
        Syscall($syscall_id, vec![$($arg),*])
    };

    ($(($syscall_id:expr, [$($arg:expr),*])),*) => {
        Seq(vec![$(syscall_seq!($syscall_id, [$($arg),*])),*])
    };
}

#[derive(Debug, Clone)]
struct Stack {
    vec: Vec<usize>,
}

impl Stack {
    fn new() -> Self {
        Stack { vec: vec![] }
    }
    fn push(&mut self) -> usize {
        let l = self.vec.len();
        self.vec.push(l);
        l
    }

    fn pop(&mut self) -> usize {
        self.vec.pop().unwrap();
        self.vec.len()
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(i32),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Syscall(u32, Vec<Expr>),
    // md5
    // .string: s....
    CString(String),
    Mem(u64),
    Seq(Vec<Expr>),
    Raw(String),
    Debug,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Asm {
    Add(usize, usize),
    Mul(usize, usize),
    Mov(usize, usize),
    Syscall(),
    Store(usize, i32),
    LoadBlob(usize, String),
    Blob(String),
    Mem(usize, u64),
    Raw(String),
    Debug,
}

pub struct MachineProgram {
    pub code: Expr,
    pub blobs: Vec<String>,
    pub assembly_str: String,
}

impl MachineProgram {
    pub fn new(code: Expr) -> Self {
        let mut argstack = Stack::new();
        let mut memstack = Vec::new();
        let asm = code.emit(&mut argstack, &mut memstack);
        let asm_str = asm
            .into_iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join("\n");
        let blobs = code
            .blobs()
            .into_iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>();

        let blobs_str = blobs.join("\n");

        let head = ".intel_syntax noprefix";

        let assembly_str = format!("{}\n{}\n{}", head, asm_str, blobs_str);

        MachineProgram {
            code,
            blobs,
            assembly_str,
        }
    }

    pub fn to_machinecode(&self) -> Vec<u8> {
        let mut f = std::fs::File::create("/tmp/a.s").unwrap();
        f.write_all(self.assembly_str.as_bytes()).unwrap();

        std::process::Command::new("gcc")
            .arg("-nostdlib")
            .arg("/tmp/a.s")
            .arg("-o")
            .arg("/tmp/a.s.elf")
            .output()
            .unwrap();

        std::process::Command::new("objcopy")
            .arg("--dump-section")
            .arg(".text=/tmp/a.s.bin")
            .arg("/tmp/a.s.elf")
            .output()
            .unwrap();

        std::fs::read("/tmp/a.s.bin").unwrap()
    }

    pub fn to_fn(&self) -> fn() -> u64 {
        unsafe {
            let vec = self.to_machinecode();
            let addr = vec.as_ptr();
            let exec_mem = libc::mmap(
                std::ptr::null_mut(),
                vec.len() + 0x10,
                libc::PROT_EXEC | libc::PROT_WRITE | libc::PROT_READ,
                libc::MAP_PRIVATE | libc::MAP_ANON,
                -1,
                0,
            );
            std::ptr::copy_nonoverlapping(addr, exec_mem as *mut u8, vec.len());
            let func: fn() -> u64 = std::mem::transmute(exec_mem);
            func
        }
    }
}

impl std::fmt::Display for Asm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use std::collections::HashMap;
        let mut regs: HashMap<usize, &str> = HashMap::new();
        regs.insert(0, "rbx");
        //regs.insert(1, "rcx");
        //regs.insert(1, "r11");
        regs.insert(1, "r12");
        regs.insert(2, "r13");
        regs.insert(3, "r14");
        regs.insert(4, "r15");

        regs.insert(0 + SYSCALL_OFFSET, "rax");
        regs.insert(1 + SYSCALL_OFFSET, "rdi");
        regs.insert(2 + SYSCALL_OFFSET, "rsi");
        regs.insert(3 + SYSCALL_OFFSET, "rdx");
        regs.insert(4 + SYSCALL_OFFSET, "r10");
        regs.insert(5 + SYSCALL_OFFSET, "r8");
        regs.insert(6 + SYSCALL_OFFSET, "r9");

        write!(
            f,
            "{}",
            match self {
                Asm::Add(a, b) => format!("add {}, {}", regs[a.min(b)], regs[b.max(a)]),
                Asm::Mul(a, b) => format!("mul {}, {}", regs[a.min(b)], regs[b.max(a)]),
                Asm::Store(r, x) => {
                    // This optimization reduces the binary size
                    // `xor R, R` is shorter than `mov R, 0`
                    if *x == 0 {
                        format!("xor {}, {}", regs[r], regs[r])
                    } else {
                        format!("mov {}, {}", regs[r], x)
                    }
                }
                Asm::LoadBlob(r, s) => {
                    let h = format!("{:x}", md5::compute(s.as_bytes()));
                    format!("lea {}, [rip+blob_{}]", regs[r], h)
                }
                Asm::Blob(s) => {
                    let h = format!("{:x}", md5::compute(s.as_bytes()));
                    format!("blob_{}:\n.string {:?}", h, s)
                }
                Asm::Mov(a, b) => {
                    if a != b {
                        format!("mov {}, {}", regs[a], regs[b])
                    } else {
                        "".to_string()
                    }
                }
                Asm::Syscall() => "syscall".to_string(),
                Asm::Mem(r, sz) => format!("lea {}, [rsp-{}]", regs[r], sz),
                Asm::Debug => String::from("int3"),
                Asm::Raw(s) => s.to_string(),
            }
        )
    }
}

impl Expr {
    fn emit(&self, regstack: &mut Stack, memstack: &mut Vec<u64>) -> Vec<Asm> {
        eprintln!("[{:?}] [{:?}]", self, regstack);
        match self {
            Expr::Debug => {
                vec![Asm::Debug]
            }
            Expr::Raw(s) => {
                vec![Asm::Raw(s.to_string())]
            }
            Expr::Literal(n) => {
                let mut instructions = vec![];
                instructions.push(Asm::Store(regstack.push(), *n));
                eprintln!("emmited {:?}", instructions);
                instructions
            }
            Expr::Add(a, b) => {
                let mut instructions = vec![];

                instructions.append(&mut a.emit(regstack, memstack));
                instructions.append(&mut b.emit(regstack, memstack));

                instructions.push(Asm::Add(regstack.pop(), regstack.pop()));
                regstack.push();
                eprintln!("emmited {:?}", instructions);
                instructions
            }
            Expr::Syscall(n, args) => {
                let mut instructions = vec![];

                // First we emit all the args, and their value will be pushed to the stack
                args.iter().enumerate().rev().for_each(|(_i, arg)| {
                    instructions.append(&mut arg.emit(regstack, memstack));
                });

                // After the args are ran, we pop the stack to the syscalls registers
                args.iter().enumerate().for_each(|(i, _)| {
                    instructions.push(Asm::Mov(i + 1 + SYSCALL_OFFSET, regstack.pop()));
                });

                instructions.push(Asm::Store(0 + SYSCALL_OFFSET, *n as i32));
                instructions.push(Asm::Syscall());
                instructions.push(Asm::Mov(regstack.push(), 0 + SYSCALL_OFFSET));

                eprintln!("emmited {:?}", instructions);
                instructions
            }
            Expr::CString(s) => {
                let d = vec![Asm::LoadBlob(regstack.push(), s.to_string())];
                d
            }
            Expr::Mem(sz) => {
                let mut instructions = vec![];
                memstack.push(memstack.last().unwrap_or(&0) + sz);
                let offset = memstack.last().unwrap();
                instructions.push(Asm::Mem(regstack.push(), *offset));
                instructions
            }
            Expr::Mul(a, b) => {
                let mut instructions = vec![];

                instructions.append(&mut Expr::emit(a, regstack, memstack));
                instructions.append(&mut Expr::emit(b, regstack, memstack));

                instructions.push(Asm::Mul(regstack.pop(), regstack.pop()));
                regstack.push();
                eprintln!("emmited {:?}", instructions);
                instructions
            }
            Expr::Seq(es) => es.iter().flat_map(|e| e.emit(regstack, memstack)).collect(),
        }
    }

    fn all_blobs(program: &[Self]) -> Vec<Asm> {
        let mut bs = program.iter().flat_map(|x| x.blobs()).collect::<Vec<_>>();
        bs.sort();
        bs.dedup();
        bs
    }

    fn blobs(&self) -> Vec<Asm> {
        match self {
            Expr::CString(s) => vec![Asm::Blob(s.to_string())],
            Expr::Add(a, b) => {
                let mut v = vec![];
                v.append(&mut Expr::blobs(a));
                v.append(&mut Expr::blobs(b));
                v.sort();
                v.dedup();
                v
            }
            Expr::Mul(a, b) => {
                let mut v = vec![];
                v.append(&mut Expr::blobs(a));
                v.append(&mut Expr::blobs(b));
                v.sort();
                v.dedup();
                v
            }
            Expr::Syscall(_, args) => Self::all_blobs(args),
            Expr::Literal(_) => vec![],
            Expr::Mem(_) => vec![],
            Expr::Debug => vec![],
            Expr::Raw(_) => vec![],
            Expr::Seq(es) => Self::all_blobs(es),
        }
    }
}

fn main() {
    use Expr::*;
    let program = Seq(vec![
        //sendfile
        Syscall(
            0x28,
            vec![
                Literal(1),
                //open
                Syscall(0x2, vec![CString!("/etc/passwd"), Literal(0)]),
                Literal(0),
                Literal(0x1122),
            ],
        ),
        //exit
        //Syscall(0x3c, vec![Literal(69)]),
        Raw!("ret"),
    ]);

    eprintln!("{:?}", program);
    let result = MachineProgram::new(program).to_fn()();
    println!("\n\nresult: {}", result);
}
