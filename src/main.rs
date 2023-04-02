mod cpu;
mod sdlgui;
mod test;

use clap::Parser;
use cpu::Cpu;
use sdlgui::SDLGui;

/// 8080 Emulator in Rust
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// ROM file to load
    #[arg()]
    rom_file: String,

    /// Graphics scale
    #[arg(default_value_t = 20)]
    scale: u32,
}

pub fn main() {
    let args = Args::parse();
    let rng = rand::random::<u8>;

    let mut cpu = Cpu::new();
    cpu.load_rom(&args.rom_file, 0);

    // let mut gui = SDLGui::new(cpu, args.scale);
    // gui.run();

    test::main();
}
