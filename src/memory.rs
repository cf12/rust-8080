use std::{
    fs,
    ops::{Index, IndexMut},
    process::Output,
};

pub struct Memory {
    pub ram: Vec<u8>,
}

impl Index<u16> for Memory {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        &self.ram[index as usize]
    }
}

impl IndexMut<u16> for Memory {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        &mut self.ram[index as usize]
    }
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            ram: vec![0; 0x10000],
        }
    }

    pub fn load_rom(&mut self, path: &String) {
        let rom = fs::read(path).expect("Cannot read ROM file");
        self.ram[..rom.len()].copy_from_slice(&rom);
    }

    pub fn disas(self) {
        let mut pc = 0;

        while pc < self.ram.len() {
            let mut opbytes = 1;
            let op = self.ram[pc];
            let b1 = self.ram[pc + 1];
            let b2 = self.ram[pc + 2];

            print!("{:04X} ", pc);

            match op {
                0x00 => {
                    println!("NOP   ")
                }
                0x01 => {
                    println!("LXI   B, ${:02X}{:02X}", b2, b1);
                    opbytes = 3;
                }
                0x02 => {
                    println!("STAX  B");
                }
                0x03 => {
                    println!("INX   B");
                }
                0x04 => {
                    println!("INR   B");
                }
                0x05 => {
                    println!("DCR   B");
                }
                0x06 => {
                    println!("MVI   B, #${:02x}", b1);
                    opbytes = 2;
                }
                0x07 => {
                    println!("RLC");
                }
                0x08 => {
                    println!("NOP");
                }
                0x09 => {
                    println!("DAD   B");
                }
                0x0A => {
                    println!("LDAX  B");
                }
                0x0B => {
                    println!("DCX   B");
                }
                0x0C => {
                    println!("INR   C");
                }
                0x0D => {
                    println!("DCR   C");
                }
                0x0E => {
                    println!("MVI   C, ${:02X}", b1);
                    opbytes = 2;
                }
                0x0F => {
                    println!("RRC");
                }
                0x10 => {
                    println!("NOP");
                }
                0x11 => {
                    println!("LXI   D, #${:02X}{:02X}", b2, b1);
                    opbytes = 3;
                }
                0x12 => {
                    println!("STAX  D");
                }
                0x13 => {
                    println!("INX   D");
                }
                0x14 => {
                    println!("INR   D");
                }
                0x15 => {
                    println!("DCR   D");
                }
                0x16 => {
                    println!("MVI   D, #${:02X}", b1);
                    opbytes = 2;
                }
                0x17 => {
                    println!("RAL");
                }
                0x18 => {
                    println!("NOP");
                }
                0x19 => {
                    println!("DAD   D");
                }
                0x1A => {
                    println!("LDAX  D");
                }
                0x1B => {
                    println!("DCX   D");
                }
                0x1C => {
                    println!("INR   E");
                }
                0x1D => {
                    println!("DCR   E");
                }
                0x1E => {
                    println!("MVI   E, #${:02X}", b1);
                    opbytes = 2;
                }
                0x1F => {
                    println!("RAR");
                }
                0x20 => {
                    println!("NOP");
                }
                0x21 => {
                    println!("LXI   H, #${:02X}{:02X}", b2, b1);
                }
                0x22 => {
                    println!("SHLD  #${:02X}{:02X}", b2, b1);
                    opbytes = 3;
                }
                0x23 => {
                    println!("INX   H");
                }
                0x24 => {
                    println!("INR   H");
                }
                0x25 => {
                    println!("DCR   H");
                }
                0x26 => {
                    println!("MVI   H, #${:02X}", b1);
                    opbytes = 2;
                }
                0x27 => {
                    println!("DAA");
                }
                0x28 => {
                    println!("NOP");
                }
                0x29 => {
                    println!("DAD   H");
                }
                0x2A => {
                    println!("LHLD  #${:02X}{:02X}", b2, b1);
                    opbytes = 3;
                }
                0x2B => {
                    println!("DCX   H");
                }
                0x2C => {
                    println!("INR   L");
                }
                0x2D => {
                    println!("DCR   L");
                }
                0x2E => {
                    println!("MVI   L, #${:02X}", b1);
                    opbytes = 2;
                }
                0x2F => {
                    println!("CMA");
                }
                0x30 => {
                    println!("NOP");
                }
                0x31 => {
                    println!("LXI   SP, #${:02X}{:02X}", b2, b1);
                    opbytes = 3;
                }
                0x32 => {
                    println!("STA   ${:02X}{:02X}", b2, b1);
                    opbytes = 3;
                }
                0x33 => {
                    println!("INX   SP");
                }
                0x34 => {
                    println!("INR   M");
                }
                0x35 => {
                    println!("DCR   M");
                }
                0x36 => {
                    println!("MVI   M, #${:02X}", b1);
                    opbytes = 2;
                }
                0x37 => {
                    println!("STC");
                }
                0x38 => {
                    println!("NOP");
                }
                0x39 => {
                    println!("DAD   SP");
                }
                0x3A => {
                    println!("LDA   #${:02X}{:02X}", b2, b1);
                    opbytes = 3;
                }
                0x3B => {
                    println!("DCX   SP");
                }
                0x3C => {
                    println!("INR   A");
                }
                0x3D => {
                    println!("DCR   A");
                }
                0x3E => {
                    println!("MVI   A, #${:02X}", b1);
                    opbytes = 2;
                }
                0x3F => {
                    println!("CMC");
                }
                0x40 => {
                    println!("MOV   B, B");
                }
                0x41 => {
                    println!("MOV   B, C");
                }
                0x42 => {
                    println!("MOV   B, D");
                }
                0x43 => {
                    println!("MOV   B, E");
                }
                0x44 => {
                    println!("MOV   B, H");
                }
                0x45 => {
                    println!("MOV   B, L");
                }
                0x46 => {
                    println!("MOV   B, M");
                }
                0x47 => {
                    println!("MOV   B, A");
                }
                0x48 => {
                    println!("MOV   C, B");
                }
                0x49 => {
                    println!("MOV   C, C");
                }
                0x4A => {
                    println!("MOV   C, D");
                }
                0x4B => {
                    println!("MOV   C, E");
                }
                0x4C => {
                    println!("MOV   C, H");
                }
                0x4D => {
                    println!("MOV   C, L");
                }
                0x4E => {
                    println!("MOV   C, M");
                }
                0x4F => {
                    println!("MOV   C, A");
                }
                0x50 => {
                    println!("MOV   D, B");
                }
                0x51 => {
                    println!("MOV   D, C");
                }
                0x52 => {
                    println!("MOV   D, D");
                }
                0x53 => {
                    println!("MOV   D, E");
                }
                0x54 => {
                    println!("MOV   D, H");
                }
                0x55 => {
                    println!("MOV   D, L");
                }
                0x56 => {
                    println!("MOV   D, M");
                }
                0x57 => {
                    println!("MOV   D, A");
                }
                0x58 => {
                    println!("MOV   E, B");
                }
                0x59 => {
                    println!("MOV   E, C");
                }
                0x5A => {
                    println!("MOV   E, D");
                }
                0x5B => {
                    println!("MOV   E, E");
                }
                0x5C => {
                    println!("MOV   E, H");
                }
                0x5D => {
                    println!("MOV   E, L");
                }
                0x5E => {
                    println!("MOV   E, M");
                }
                0x5F => {
                    println!("MOV   E, A");
                }
                0x60 => {
                    println!("MOV   H, B");
                }
                0x61 => {
                    println!("MOV   H, C");
                }
                0x62 => {
                    println!("MOV   H, D");
                }
                0x63 => {
                    println!("MOV   H, E");
                }
                0x64 => {
                    println!("MOV   H, H");
                }
                0x65 => {
                    println!("MOV   H, L");
                }
                0x66 => {
                    println!("MOV   H, M");
                }
                0x67 => {
                    println!("MOV   H, A");
                }
                0x68 => {
                    println!("MOV   L, B");
                }
                0x69 => {
                    println!("MOV   L, C");
                }
                0x6A => {
                    println!("MOV   L, D");
                }
                0x6B => {
                    println!("MOV   L, E");
                }
                0x6C => {
                    println!("MOV   L, H");
                }
                0x6D => {
                    println!("MOV   L, L");
                }
                0x6E => {
                    println!("MOV   L, M");
                }
                0x6F => {
                    println!("MOV   L, A");
                }
                0x70 => {
                    println!("MOV   M, B");
                }
                0x71 => {
                    println!("MOV   M, C");
                }
                0x72 => {
                    println!("MOV   M, D");
                }
                0x73 => {
                    println!("MOV   M, E");
                }
                0x74 => {
                    println!("MOV   M, H");
                }
                0x75 => {
                    println!("MOV   M, L");
                }
                0x76 => {
                    println!("HLT");
                }
                0x77 => {
                    println!("MOV   M, A");
                }
                0x78 => {
                    println!("MOV   A, B");
                }
                0x79 => {
                    println!("MOV   A, C");
                }
                0x7A => {
                    println!("MOV   A, D");
                }
                0x7B => {
                    println!("MOV   A, E");
                }
                0x7C => {
                    println!("MOV   A, H");
                }
                0x7D => {
                    println!("MOV   A, L");
                }
                0x7E => {
                    println!("MOV   A, M");
                }
                0x7F => {
                    println!("MOV   A, A");
                }
                0x80 => {
                    println!("ADD   B");
                }
                0x81 => {
                    println!("ADD   C");
                }
                0x82 => {
                    println!("ADD   D");
                }
                0x83 => {
                    println!("ADD   E");
                }
                0x84 => {
                    println!("ADD   H");
                }
                0x85 => {
                    println!("ADD   L");
                }
                0x86 => {
                    println!("ADD   M");
                }
                0x87 => {
                    println!("ADD   A");
                }
                0x88 => {
                    println!("ADC   B");
                }
                0x89 => {
                    println!("ADC   C");
                }
                0x8A => {
                    println!("ADC   D");
                }
                0x8B => {
                    println!("ADC   E");
                }
                0x8C => {
                    println!("ADC   H");
                }
                0x8D => {
                    println!("ADC   L");
                }
                0x8E => {
                    println!("ADC   M");
                }
                0x8F => {
                    println!("ADC   A");
                }
                0x90 => {
                    println!("SUB   B");
                }
                0x91 => {
                    println!("SUB   C");
                }
                0x92 => {
                    println!("SUB   D");
                }
                0x93 => {
                    println!("SUB   E");
                }
                0x94 => {
                    println!("SUB   H");
                }
                0x95 => {
                    println!("SUB   L");
                }
                0x96 => {
                    println!("SUB   M");
                }
                0x97 => {
                    println!("SUB   A");
                }
                0x98 => {
                    println!("SBB   B");
                }
                0x99 => {
                    println!("SBB   C");
                }
                0x9A => {
                    println!("SBB   D");
                }
                0x9B => {
                    println!("SBB   E");
                }
                0x9C => {
                    println!("SBB   H");
                }
                0x9D => {
                    println!("SBB   L");
                }
                0x9E => {
                    println!("SBB   M");
                }
                0x9F => {
                    println!("SBB   A");
                }
                0xA0 => {
                    println!("ANA   B");
                }
                0xA1 => {
                    println!("ANA   C");
                }
                0xA2 => {
                    println!("ANA   D");
                }
                0xA3 => {
                    println!("ANA   E");
                }
                0xA4 => {
                    println!("ANA   H");
                }
                0xA5 => {
                    println!("ANA   L");
                }
                0xA6 => {
                    println!("ANA   M");
                }
                0xA7 => {
                    println!("ANA   A");
                }
                0xA8 => {
                    println!("XRA   B");
                }
                0xA9 => {
                    println!("XRA   C");
                }
                0xAA => {
                    println!("XRA   D");
                }
                0xAB => {
                    println!("XRA   E");
                }
                0xAC => {
                    println!("XRA   H");
                }
                0xAD => {
                    println!("XRA   L");
                }
                0xAE => {
                    println!("XRA   M");
                }
                0xAF => {
                    println!("XRA   A");
                }
                0xB0 => {
                    println!("ORA   B");
                }
                0xB1 => {
                    println!("ORA   C");
                }
                0xB2 => {
                    println!("ORA   D");
                }
                0xB3 => {
                    println!("ORA   E");
                }
                0xB4 => {
                    println!("ORA   H");
                }
                0xB5 => {
                    println!("ORA   L");
                }
                0xB6 => {
                    println!("ORA   M");
                }
                0xB7 => {
                    println!("ORA   A");
                }
                0xB8 => {
                    println!("CMP   B");
                }
                0xB9 => {
                    println!("CMP   C");
                }
                0xBA => {
                    println!("CMP   D");
                }
                0xBB => {
                    println!("CMP   E");
                }
                0xBC => {
                    println!("CMP   H");
                }
                0xBD => {
                    println!("CMP   L");
                }
                0xBE => {
                    println!("CMP   M");
                }
                0xBF => {
                    println!("CMP   A");
                }
                0xC0 => {
                    println!("RNZ");
                }
                0xC1 => {
                    println!("POP   B");
                }
                0xC2 => {
                    println!("JNZ   ${:02x}{:02x}", b2, b1);
                    opbytes = 3;
                }
                0xC3 => {
                    println!("JMP   ${:02x}{:02x}", b2, b1);
                    opbytes = 3;
                }
                0xC4 => {
                    println!("CNZ   ${:02x}{:02x}", b2, b1);
                    opbytes = 3;
                }
                0xC5 => {
                    println!("PUSH  B");
                }
                0xC6 => {
                    println!("ADI   {:#02X}", self.ram[pc + 1]);
                }
                0xC7 => {
                    println!("RST   0");
                }
                0xC8 => {
                    println!("RZ");
                }
                0xC9 => {
                    println!("RET");
                }
                0xCA => {
                    println!("JZ    ${:02X}{:02X}", b2, b1);
                    opbytes = 3;
                }
                0xCB => {
                    println!("JMP   ${:02X}{:02X}", b2, b1);
                    opbytes = 3;
                }
                0xCC => {
                    println!("CZ    ${:02X}{:02X}", b2, b1);
                    opbytes = 3;
                }
                0xCD => {
                    println!("CALL  ${:02X}{:02X}", b2, b1);
                    opbytes = 3;
                }
                0xCE => {
                    println!("ACI   {:#02X}", self.ram[pc + 1]);
                }
                0xCF => {
                    println!("RST  1");
                }
                0xD0 => {
                    println!("RNC");
                }
                0xD1 => {
                    println!("POP   D");
                }
                0xD2 => {
                    println!("JNC   {:#04X}", self.ram[pc + 1]);
                }
                0xD3 => {
                    println!("OUT   {:#02X}", self.ram[pc + 1]);
                }
                0xD4 => {
                    println!("CNC   {:#04X}", self.ram[pc + 1]);
                }
                0xD5 => {
                    println!("PUSH  D");
                }
                0xD6 => {
                    println!("SUI   {:#02X}", self.ram[pc + 1]);
                }
                0xD7 => {
                    println!("RST   2");
                }
                0xD8 => {
                    println!("RC");
                }
                0xD9 => {
                    println!("RET");
                }
                0xDA => {
                    println!("JC    {:#04X}", self.ram[pc + 1]);
                }
                0xDB => {
                    println!("IN    {:#02X}", self.ram[pc + 1]);
                }
                0xDC => {
                    println!("CC    {:#04X}", self.ram[pc + 1]);
                }
                0xDD => {
                    println!("CALL  {:#04X}", self.ram[pc + 1]);
                }
                0xDE => {
                    println!("SBI   {:#02X}", self.ram[pc + 1]);
                }
                0xDF => {
                    println!("RST   3");
                }
                0xE0 => {
                    println!("RPO");
                }
                0xE1 => {
                    println!("POP   H");
                }
                0xE2 => {
                    println!("JPO   {:#04X}", self.ram[pc + 1]);
                }
                0xE3 => {
                    println!("XTHL");
                }
                0xE4 => {
                    println!("CPO   {:#04X}", self.ram[pc + 1]);
                }
                0xE5 => {
                    println!("PUSH  H");
                }
                0xE6 => {
                    println!("ANI   {:#02X}", self.ram[pc + 1]);
                }
                0xE7 => {
                    println!("RST   4");
                }
                0xE8 => {
                    println!("RPE");
                }
                0xE9 => {
                    println!("PCHL");
                }
                0xEA => {
                    println!("JPE   {:#04X}", self.ram[pc + 1]);
                }
                0xEB => {
                    println!("XCHG");
                }
                0xEC => {
                    println!("CPE   {:#04X}", self.ram[pc + 1]);
                }
                0xED => {
                    println!("CALL  {:#04X}", self.ram[pc + 1]);
                }
                0xEE => {
                    println!("XRI   {:#02X}", self.ram[pc + 1]);
                }
                0xEF => {
                    println!("RST   5");
                }
                0xF0 => {
                    println!("RP");
                }
                0xF1 => {
                    println!("POP   PSW");
                }
                0xF2 => {
                    println!("JP    {:#04X}", self.ram[pc + 1]);
                }
                0xF3 => {
                    println!("DI");
                }
                0xF4 => {
                    println!("CP    {:#04X}", self.ram[pc + 1]);
                }
                0xF5 => {
                    println!("PUSH  PSW");
                }
                0xF6 => {
                    println!("ORI   {:#02X}", self.ram[pc + 1]);
                }
                0xF7 => {
                    println!("RST   6");
                }
                0xF8 => {
                    println!("RM");
                }
                0xF9 => {
                    println!("SPHL");
                }
                0xFA => {
                    println!("JM    {:#04X}", self.ram[pc + 1]);
                }
                0xFB => {
                    println!("EI");
                }
                0xFC => {
                    println!("CM    {:#04X}", self.ram[pc + 1]);
                }
                0xFD => {
                    println!("CALL  {:#04X}", self.ram[pc + 1]);
                }
                0xFE => {
                    println!("CPI   {:#02X}", self.ram[pc + 1]);
                }
                0xFF => {
                    println!("RST   7");
                }
            }

            pc += opbytes;
        }

        println!("{:?}", self.ram);
    }
}
