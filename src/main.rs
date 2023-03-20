use std::io::stdin;

use clap::Parser;
use memory::Memory;
use cpu::Cpu;

mod memory;
mod cpu;

/// 8080 Emulator in Rust
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// ROM file to load
    #[arg()]
    rom_file: String,

    // /// Graphics scale
    // #[arg(default_value_t = 20)]
    // scale: u32,
}

pub fn main() {
    let args = Args::parse();
    let rng = rand::random::<u8>;
    // let mut cpu = Chip8::new(rng);
    // cpu.load_rom(&args.rom_file);
    // let mut gui = SDLGui::new(cpu, args.scale);
    // gui.run();

    let mut cpu = Cpu::new();
    cpu.load_rom(&args.rom_file);

    loop {
        let mut line = String::new();
        stdin().read_line(&mut line).unwrap();

        cpu.cycle();
        println!("{}", cpu);
    }
}