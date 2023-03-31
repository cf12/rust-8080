use crate::cpu::Cpu;

pub struct Test {
    cpu: Cpu,
    done: bool,
}

impl Test {
    pub fn new() -> Self {
        let this = Test { Cpu: None, done: false };

        let mut cpu = Cpu::new(
            &(this.port_in),
            &(this.port_out),
        );
        cpu.load_rom(&String::from("roms/test"));

        this
    }

    fn port_in(port: u8) -> u8 {
        0x00
    }

    fn port_out(&mut self, port: u8, value: u8) {
        match port {
            0x0 => self.done = true,
            0x1 => {
                let op = self.cpu.c;

                match op {
                    0x2 => print!("{}", self.cpu.e as char),
                    0x9 => {
                        let mut addr = self.cpu.de();

                        while self.cpu.mem[addr as usize] as char != '$' {
                            print!("{}", self.cpu.mem[addr as usize]);
                            addr += 1;
                        }
                    }
                }
            }
        }
    }
}
