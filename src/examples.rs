#[cfg(test)]
mod tests {

    use crate::CString;
    use crate::Expr::*;

    fn exit() {
        let program = Seq(vec![Syscall(
            0x3c,
            vec![Add(Box::new(Literal(1)), Box::new(Literal(68)))],
        )]);
    }

    #[test]
    fn hello() {
        let program = Seq(vec![
            Syscall(
                0x1,
                vec![Literal(1), CString!("Hello world!\n"), Literal(13)],
            ),
            Syscall(0x3c, vec![Add(Box::new(Literal(1)), Box::new(Literal(60)))]),
        ]);

        println!("{:?}", program);
        assert_eq!(1, 1);
    }
}
