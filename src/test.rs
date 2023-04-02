use std::{path::Path, io};

use crate::cpu::Cpu;

fn test(path: impl AsRef<Path>) {
    let mut cpu = Cpu::new();

    cpu.load_rom(path, 0x100);

    cpu.mem[0x0005] = 0xc9;
    cpu.pc = 0x100;

    println!("{}", cpu);

    loop {
        let mut buffer = String::new();
        let stdin = io::stdin(); // We get `Stdin` here.
        stdin.read_line(&mut buffer).unwrap();

        cpu.cycle();
        println!("{}", cpu);

        match cpu.pc {
            0x00 => break,
            0x05 => {
                let op = cpu.c;

                match op {
                    0x2 => print!("{}", cpu.e as char),
                    0x9 => {
                        let mut addr = cpu.de();

                        while cpu.mem[addr as usize] as char != '$' {
                            print!("{}", cpu.mem[addr as usize] as char);
                            addr += 1;
                        }
                    }
                    _ => (),
                }
            }
            _ => (),
        }
    }
}

pub fn main() {
    test("./roms/TST8080.COM");
}
