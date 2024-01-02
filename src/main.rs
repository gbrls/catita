const SYSCALL_OFFSET: usize = 100;

#[macro_export]
macro_rules! CString {
    ($str:expr) => {
        CString(String::from($str))
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
enum Expr {
    Literal(i32),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Syscall(u32, Vec<Expr>),
    // md5
    // .string: s....
    CString(String),
    Mem(u64),
    Seq(Vec<Expr>),
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
    //PopSlot(u32),
}

impl std::fmt::Display for Asm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use std::collections::HashMap;
        let mut regs: HashMap<usize, &str> = HashMap::new();
        regs.insert(0, "rbx");
        regs.insert(1, "rcx");
        regs.insert(2, "r11");
        regs.insert(3, "r12");
        regs.insert(4, "r13");
        regs.insert(5, "r14");
        regs.insert(6, "r15");

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
                Asm::Store(r, x) => format!("mov {}, {}", regs[r], x),
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
                //Asm::PopSlot(sz) => format!("add rsp, {}", sz),
            }
        )
    }
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

impl Expr {
    fn emit(&self, regstack: &mut Stack, memstack: &mut Vec<u64>) -> Vec<Asm> {
        eprintln!("[{:?}] [{:?}]", self, regstack);
        match self {
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
            Expr::Seq(es) => Self::all_blobs(es),
        }
    }
}

fn main() {
    use Expr::*;
    let mut argstack = Stack::new();
    let mut memstack = Vec::new();
    //let program = Add(
    //    Box::new(CString(String::from("Hello there"))),
    //    Box::new(Mul(Box::new(Literal(33)), Box::new(Literal(44)))),
    //);

    //let program = Seq(vec![syscall_seq![
    //    (0x1, [Literal(1), CString!("Hello world!\n"), Literal(13)]),
    //    //(0x3c, [Literal(69)])
    //    (0x3c, [Add(Box::new(Literal(1)), Box::new(Literal(60)))]),
    //]]);
    let environ_read_len = 0x30;
    let program = Seq(vec![
        Mem(0x10),
        Mem(0x10),
        //sendfile
        Syscall(
            0x28,
            vec![
                Literal(1),
                //open
                Syscall(0x2, vec![CString!("/etc/passwd"), Literal(0)]),
                Literal(0),
                Literal(0x300),
            ],
        ),
        // read
        Syscall(
            0x0,
            vec![
                Syscall(0x2, vec![CString!("/proc/self/environ"), Literal(0)]),
                // This argument below is not working
                Mem(environ_read_len + 2),
                Literal(environ_read_len as i32),
            ],
        ),
        //exit
        Syscall(0x3c, vec![Literal(69)]),
    ]);

    eprintln!("{:?}", program);

    let code = program.emit(&mut argstack, &mut memstack);
    let code = code.into_iter().map(|x| x.to_string()).collect::<Vec<_>>();
    let blobs = program
        .blobs()
        .into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>();

    let code = code.join("\n");
    let blobs = blobs.join("\n");
    let head = ".intel_syntax noprefix";

    println!("{}\n{}\n{}", head, code, blobs);
}
