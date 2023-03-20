use core::fmt;
use std::process::exit;

use crate::Memory;

// TODO: use addr type
type Addr = u16;

// TODO: use bitfield crate maybe
pub struct CpuFlags {
    z: bool,
    s: bool,
    p: bool,
    cy: bool,
    ac: bool,
    pad: u8,
}

pub struct Cpu {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,

    sp: u16,
    pc: u16,

    mem: Memory,
    cc: CpuFlags,

    inte: bool,
}

impl CpuFlags {
    fn encode_u8(&self) -> u8 {
        return (self.z as u8)
            | ((self.s as u8) << 1)
            | ((self.p as u8) << 2)
            | ((self.cy as u8) << 3)
            | ((self.ac as u8) << 4);
    }

    fn decode_u8(&mut self, num: u8) {
        self.z = (num & 0b1) == 0b1;
        self.s = (num & 0b10) == 0b10;
        self.p = (num & 0b100) == 0b100;
        self.cy = (num & 0b1000) == 0b1000;
        self.ac = (num & 0b10000) == 0b10000;
    }
}

impl fmt::Display for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[a]: {:02X} | ", self.a)?;
        write!(f, "[bc]: {:02X}{:02X} | ", self.b, self.c)?;
        write!(f, "[de]: {:02X}{:02X} | ", self.d, self.e)?;
        write!(f, "[hl]: {:02X}{:02X}\n", self.h, self.l)?;
        write!(f, "[sp]: {:04X} | ", self.sp)?;
        write!(f, "[pc]: {:04X} | ", self.pc)?;
        write!(f, "[op]: {:02X}\n", self.mem[self.pc])
    }
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,

            sp: 0,
            pc: 0,

            mem: Memory::new(),
            cc: CpuFlags {
                z: false,
                s: false,
                p: false,
                cy: false,
                ac: false,
                pad: 0,
            },

            inte: false,
        }
    }

    pub fn load_rom(&mut self, path: &String) {
        self.mem.load_rom(path);
    }

    fn next_byte(&mut self) -> u8 {
        self.pc += 1;
        return self.mem[self.pc - 1];
    }

    fn next_word(&mut self) -> u16 {
        self.pc += 2;
        // little endian
        return ((self.mem[self.pc - 1] as u16) << 8) | (self.mem[self.pc - 2] as u16);
    }

    pub fn cycle(&mut self) {
        let op = self.next_byte();

        // TODO: optimize these calls to only occur in ops
        let hl = ((self.h as u16) << 8) | self.l as u16;
        let bc = ((self.b as u16) << 8) | self.c as u16;
        let de = ((self.d as u16) << 8) | self.e as u16;

        match op {
            // 0x00	NOP	1
            0x00 => {}
            // 0x01	LXI B,D16	3		B <- byte 3, C <- byte 2
            0x01 => {
                self.c = self.next_byte();
                self.b = self.next_byte();
            }
            // 0x02	STAX B	1		(BC) <- A
            // 0x03	INX B	1		BC <- BC+1
            // 0x04	INR B	1	Z, S, P, AC	B <- B+1
            0x04 => {
                self.b += 1;
                self.update_cc(self.b);
            }

            // 0x05	DCR B	1	Z, S, P, AC	B <- B-1
            0x05 => {
                self.b -= 1;
                self.update_cc(self.b);
            }
            // 0x0d	DCR C	1	Z, S, P, AC	C <-C-1
            0x0D => {
                self.c -= 1;
                self.update_cc(self.c);
            }
            // 0x15	DCR D	1	Z, S, P, AC	D <- D-1
            0x15 => {
                self.d -= 1;
                self.update_cc(self.d)
            }
            // 0x1d	DCR E	1	Z, S, P, AC	E <- E-1
            0x1D => {
                self.e -= 1;
                self.update_cc(self.e)
            }
            // 0x25	DCR H	1	Z, S, P, AC	H <- H-1
            0x25 => {
                self.h -= 1;
                self.update_cc(self.h)
            }
            // 0x2d	DCR L	1	Z, S, P, AC	L <- L-1
            0x2D => {
                self.l -= 1;
                self.update_cc(self.l)
            }
            // 0x35	DCR M	1	Z, S, P, AC	(HL) <- (HL)-1
            0x35 => {
                self.mem[hl] -= 1;
                self.update_cc(self.mem[hl])
            }
            // 0x3d	DCR A	1	Z, S, P, AC	A <- A-1
            0x3D => {
                self.a -= 1;
                self.update_cc(self.a);
            }

            // 0x06	MVI B, D8	2		B <- byte 2
            0x06 => {
                self.b = self.next_byte();
            }
            // 0x0e	MVI C,D8	2		C <- byte 2
            0x0E => {
                self.c = self.next_byte();
            }
            // 0x16	MVI D, D8	2		D <- byte 2
            0x16 => {
                self.d = self.next_byte();
            }
            // 0x1e	MVI E,D8	2		E <- byte 2
            0x1E => {
                self.e = self.next_byte();
            }
            // 0x26	MVI H,D8	2		H <- byte 2
            0x26 => {
                self.h = self.next_byte();
            }
            // 0x2e	MVI L, D8	2		L <- byte 2
            0x2E => {
                self.l = self.next_byte();
            }
            // 0x36	MVI M,D8	2		(HL) <- byte 2
            0x36 => {
                self.mem[hl] = self.next_byte();
            }
            // 0x3e	MVI A,D8	2		A <- byte 2
            0x3E => {
                self.a = self.next_byte();
            }

            // 0x07	RLC	1	CY	A = A << 1; bit 0 = prev bit 7; CY = prev bit 7
            // 0x08	-

            // 0x09	DAD B	1	CY	HL = HL + BC
            0x09 => {
                let (res, cy) = hl.overflowing_add(bc);
                self.h = (res >> 8) as u8;
                self.l = (res & 0x0F) as u8;
                self.cc.cy = cy;
            }
            // 0x19	DAD D	1	CY	HL = HL + DE
            // 0x29	DAD H	1	CY	HL = HL + HI
            // 0x39	DAD SP	1	CY	HL = HL + SP

            // 0x0a	LDAX B	1		A <- (BC)
            // 0x0b	DCX B	1		BC = BC-1
            // 0x0c	INR C	1	Z, S, P, AC	C <- C+1
            // 0x0f	RRC	1	CY	A = A >> 1; bit 7 = prev bit 0; CY = prev bit 0
            // 0x10	-
            // 0x11	LXI D,D16	3		D <- byte 3, E <- byte 2
            // 0x12	STAX D	1		(DE) <- A
            // 0x13	INX D	1		DE <- DE + 1
            // 0x14	INR D	1	Z, S, P, AC	D <- D+1
            // 0x17	RAL	1	CY	A = A << 1; bit 0 = prev CY; CY = prev bit 7
            // 0x18	-
            // 0x1a	LDAX D	1		A <- (DE)
            // 0x1b	DCX D	1		DE = DE-1
            // 0x1c	INR E	1	Z, S, P, AC	E <-E+1
            // 0x1f	RAR	1	CY	A = A >> 1; bit 7 = prev bit 7; CY = prev bit 0
            // 0x20	-
            // 0x21	LXI H,D16	3		H <- byte 3, L <- byte 2
            // 0x22	SHLD adr	3		(adr) <-L; (adr+1)<-H
            // 0x23	INX H	1		HL <- HL + 1
            // 0x24	INR H	1	Z, S, P, AC	H <- H+1
            // 0x27	DAA	1		special
            // 0x28	-
            // 0x2a	LHLD adr	3		L <- (adr); H<-(adr+1)
            // 0x2b	DCX H	1		HL = HL-1
            // 0x2c	INR L	1	Z, S, P, AC	L <- L+1
            // 0x2f	CMA	1		A <- !A
            // 0x30	-
            // 0x31	LXI SP, D16	3		SP.hi <- byte 3, SP.lo <- byte 2
            0x31 => {
                self.sp = self.next_word();
            }
            // 0x32	STA adr	3		(adr) <- A
            // 0x33	INX SP	1		SP = SP + 1
            // 0x34	INR M	1	Z, S, P, AC	(HL) <- (HL)+1
            // 0x37	STC	1	CY	CY = 1
            // 0x38	-
            // 0x3a	LDA adr	3		A <- (adr)
            // 0x3b	DCX SP	1		SP = SP-1
            // 0x3c	INR A	1	Z, S, P, AC	A <- A+1
            // 0x3f	CMC	1	CY	CY=!CY
            // 0x40	MOV B,B	1		B <- B
            0x40 => {
                self.b = self.b;
            }
            // 0x41	MOV B,C	1		B <- C
            0x41 => {
                self.b = self.c;
            }
            // 0x42	MOV B,D	1		B <- D
            0x42 => {
                self.b = self.d;
            }
            // 0x43	MOV B,E	1		B <- E
            0x43 => {
                self.b = self.e;
            }
            // 0x44	MOV B,H	1		B <- H
            0x44 => {
                self.b = self.h;
            }
            // 0x45	MOV B,L	1		B <- L
            0x45 => {
                self.b = self.l;
            }
            // 0x46	MOV B,M	1		B <- (HL)
            0x46 => {
                self.b = self.mem[hl];
            }
            // 0x47	MOV B,A	1		B <- A
            0x47 => {
                self.b = self.a;
            }
            // 0x48	MOV C,B	1		C <- B
            0x48 => {
                self.c = self.b;
            }
            // 0x49	MOV C,C	1		C <- C
            0x49 => {
                self.c = self.c;
            }
            // 0x4a	MOV C,D	1		C <- D
            0x4A => {
                self.c = self.d;
            }
            // 0x4b	MOV C,E	1		C <- E
            0x4B => {
                self.c = self.e;
            }
            // 0x4c	MOV C,H	1		C <- H
            0x4C => {
                self.c = self.h;
            }
            // 0x4d	MOV C,L	1		C <- L
            0x4D => {
                self.c = self.l;
            }
            // 0x4e	MOV C,M	1		C <- (HL)
            0x4E => {
                self.c = self.mem[hl];
            }
            // 0x4f	MOV C,A	1		C <- A
            0x4F => {
                self.c = self.a;
            }
            // 0x50	MOV D,B	1		D <- B
            0x50 => {
                self.d = self.b;
            }
            // 0x51	MOV D,C	1		D <- C
            0x51 => {
                self.d = self.c;
            }
            // 0x52	MOV D,D	1		D <- D
            0x52 => {
                self.d = self.d;
            }
            // 0x53	MOV D,E	1		D <- E
            0x53 => {
                self.d = self.e;
            }
            // 0x54	MOV D,H	1		D <- H
            0x54 => {
                self.d = self.h;
            }
            // 0x55	MOV D,L	1		D <- L
            0x55 => {
                self.d = self.l;
            }
            // 0x56	MOV D,M	1		D <- (HL)
            0x56 => {
                self.d = self.mem[hl];
            }
            // 0x57	MOV D,A	1		D <- A
            0x57 => {
                self.d = self.a;
            }
            // 0x58	MOV E,B	1		E <- B
            0x58 => self.e = self.b,
            // 0x59	MOV E,C	1		E <- C
            0x59 => self.e = self.c,
            // 0x5a	MOV E,D	1		E <- D
            0x5A => self.e = self.d,
            // 0x5b	MOV E,E	1		E <- E
            0x5B => self.e = self.e,
            // 0x5c	MOV E,H	1		E <- H
            0x5C => self.e = self.h,
            // 0x5d	MOV E,L	1		E <- L
            0x5D => self.e = self.l,
            // 0x5e	MOV E,M	1		E <- (HL)
            0x5E => self.e = self.mem[hl],
            // 0x5f	MOV E,A	1		E <- A
            0x5F => self.e = self.a,
            // 0x60	MOV H,B	1		H <- B
            0x60 => self.h = self.b,
            // 0x61	MOV H,C	1		H <- C
            0x61 => self.h = self.c,
            // 0x62	MOV H,D	1		H <- D
            0x62 => self.h = self.d,
            // 0x63	MOV H,E	1		H <- E
            0x63 => self.h = self.e,
            // 0x64	MOV H,H	1		H <- H
            0x64 => self.h = self.h,
            // 0x65	MOV H,L	1		H <- L
            0x65 => self.h = self.l,
            // 0x66	MOV H,M	1		H <- (HL)
            0x66 => self.h = self.mem[hl],
            // 0x67	MOV H,A	1		H <- A
            0x67 => self.h = self.a,
            // 0x68	MOV L,B	1		L <- B
            0x68 => self.l = self.b,
            // 0x69	MOV L,C	1		L <- C
            0x69 => self.l = self.c,
            // 0x6a	MOV L,D	1		L <- D
            0x6A => self.l = self.d,
            // 0x6b	MOV L,E	1		L <- E
            0x6B => self.l = self.e,
            // 0x6c	MOV L,H	1		L <- H
            0x6C => self.l = self.h,
            // 0x6d	MOV L,L	1		L <- L
            0x6D => self.l = self.l,
            // 0x6e	MOV L,M	1		L <- (HL)
            0x6E => self.l = self.mem[hl],
            // 0x6f	MOV L,A	1		L <- A
            0x6F => self.l = self.a,
            // 0x70	MOV M,B	1		(HL) <- B
            0x70 => self.mem[hl] = self.b,
            // 0x71	MOV M,C	1		(HL) <- C
            0x71 => self.mem[hl] = self.c,
            // 0x72	MOV M,D	1		(HL) <- D
            0x72 => self.mem[hl] = self.d,
            // 0x73	MOV M,E	1		(HL) <- E
            0x73 => self.mem[hl] = self.e,
            // 0x74	MOV M,H	1		(HL) <- H
            0x74 => self.mem[hl] = self.h,
            // 0x75	MOV M,L	1		(HL) <- L
            0x75 => self.mem[hl] = self.l,
            // 0x76	HLT	1		special
            0x76 => exit(0),
            // 0x77	MOV M,A	1		(HL) <- A
            0x77 => self.mem[hl] = self.a,
            // 0x78	MOV A,B	1		A <- B
            0x78 => self.a = self.b,
            // 0x79	MOV A,C	1		A <- C
            0x79 => self.a = self.c,
            // 0x7a	MOV A,D	1		A <- D
            0x7A => self.a = self.d,
            // 0x7b	MOV A,E	1		A <- E
            0x7B => self.a = self.e,
            // 0x7c	MOV A,H	1		A <- H
            0x7C => self.a = self.h,
            // 0x7d	MOV A,L	1		A <- L
            0x7D => self.a = self.l,
            // 0x7e	MOV A,M	1		A <- (HL)
            0x7E => self.a = self.mem[hl],
            // 0x7f	MOV A,A	1		A <- A
            0x7F => self.a = self.a,
            // ADD B
            0x80 => self.op_add(self.b),
            // ADD C
            0x81 => self.op_add(self.c),
            // ADD D
            0x82 => self.op_add(self.d),
            // ADD E
            0x83 => self.op_add(self.e),
            // ADD H
            0x84 => self.op_add(self.h),
            // ADD L
            0x85 => self.op_add(self.l),
            // ADD M
            0x86 => self.op_add(self.mem[hl]),
            // ADD A
            0x87 => self.op_add(self.a),
            // ADC B
            0x88 => self.op_add(self.b + self.cc.cy as u8),
            // ADC C
            0x89 => self.op_add(self.c + self.cc.cy as u8),
            // ADC D
            0x8A => self.op_add(self.d + self.cc.cy as u8),
            // ADC E
            0x8B => self.op_add(self.e + self.cc.cy as u8),
            // ADC H
            0x8C => self.op_add(self.h + self.cc.cy as u8),
            // ADC L
            0x8D => self.op_add(self.l + self.cc.cy as u8),
            // ADI byte
            0xC6 => {
                let byte = self.next_byte();
                self.op_add(byte);
            }
            // SUB B
            0x90 => self.op_sub(self.b),
            // SUB C	1	Z, S, P, CY, AC	A <- A - C
            0x91 => self.op_sub(self.c),
            // 0x92	SUB D	1	Z, S, P, CY, AC	A <- A + D
            0x92 => self.op_sub(self.d),
            // 0x93	SUB E	1	Z, S, P, CY, AC	A <- A - E
            0x93 => self.op_sub(self.e),
            // 0x94	SUB H	1	Z, S, P, CY, AC	A <- A + H
            0x94 => self.op_sub(self.h),
            // 0x95	SUB L	1	Z, S, P, CY, AC	A <- A - L
            0x95 => self.op_sub(self.l),
            // 0x96	SUB M	1	Z, S, P, CY, AC	A <- A + (HL)
            0x96 => self.op_sub(self.mem[hl]),
            // 0x97	SUB A	1	Z, S, P, CY, AC	A <- A - A
            0x97 => self.op_sub(self.a),
            // 0x98	SBB B	1	Z, S, P, CY, AC	A <- A - B - CY
            0x98 => self.op_sub(self.b - self.cc.cy as u8),
            // 0x99	SBB C	1	Z, S, P, CY, AC	A <- A - C - CY
            0x99 => self.op_sub(self.c - self.cc.cy as u8),
            // 0x9a	SBB D	1	Z, S, P, CY, AC	A <- A - D - CY
            0x9A => self.op_sub(self.d - self.cc.cy as u8),
            // 0x9b	SBB E	1	Z, S, P, CY, AC	A <- A - E - CY
            0x9B => self.op_sub(self.e - self.cc.cy as u8),
            // 0x9c	SBB H	1	Z, S, P, CY, AC	A <- A - H - CY
            0x9C => self.op_sub(self.h - self.cc.cy as u8),
            // 0x9d	SBB L	1	Z, S, P, CY, AC	A <- A - L - CY
            0x9D => self.op_sub(self.l - self.cc.cy as u8),
            // 0x9e	SBB M	1	Z, S, P, CY, AC	A <- A - (HL) - CY
            0x9E => self.op_sub(self.mem[hl] - self.cc.cy as u8),
            // 0x9f	SBB A	1	Z, S, P, CY, AC	A <- A - A - CY
            0x9F => self.op_sub(self.a - self.cc.cy as u8),
            // 0xa0	ANA B	1	Z, S, P, CY, AC	A <- A & B
            0xA0 => self.op_and(self.b),
            // 0xa1	ANA C	1	Z, S, P, CY, AC	A <- A & C
            0xA1 => self.op_and(self.c),
            // 0xa2	ANA D	1	Z, S, P, CY, AC	A <- A & D
            0xA2 => self.op_and(self.d),
            // 0xa3	ANA E	1	Z, S, P, CY, AC	A <- A & E
            0xA3 => self.op_and(self.e),
            // 0xa4	ANA H	1	Z, S, P, CY, AC	A <- A & H
            0xA4 => self.op_and(self.h),
            // 0xa5	ANA L	1	Z, S, P, CY, AC	A <- A & L
            0xA5 => self.op_and(self.l),
            // 0xa6	ANA M	1	Z, S, P, CY, AC	A <- A & (HL)
            0xA6 => self.op_and(self.mem[hl]),
            // 0xa7	ANA A	1	Z, S, P, CY, AC	A <- A & A
            0xA7 => self.op_and(self.a),
            // 0xa8	XRA B	1	Z, S, P, CY, AC	A <- A ^ B
            0xA8 => self.op_xor(self.b),
            // 0xa9	XRA C	1	Z, S, P, CY, AC	A <- A ^ C
            0xA9 => self.op_xor(self.c),
            // 0xaa	XRA D	1	Z, S, P, CY, AC	A <- A ^ D
            0xAA => self.op_xor(self.d),
            // 0xab	XRA E	1	Z, S, P, CY, AC	A <- A ^ E
            0xAB => self.op_xor(self.e),
            // 0xac	XRA H	1	Z, S, P, CY, AC	A <- A ^ H
            0xAC => self.op_xor(self.h),
            // 0xad	XRA L	1	Z, S, P, CY, AC	A <- A ^ L
            0xAD => self.op_xor(self.l),
            // 0xae	XRA M	1	Z, S, P, CY, AC	A <- A ^ (HL)
            0xAE => self.op_xor(self.mem[hl]),
            // 0xaf	XRA A	1	Z, S, P, CY, AC	A <- A ^ A
            0xAF => self.op_xor(self.a),
            // 0xb0	ORA B	1	Z, S, P, CY, AC	A <- A | B
            0xB0 => self.op_or(self.b),
            // 0xb1	ORA C	1	Z, S, P, CY, AC	A <- A | C
            0xB1 => self.op_or(self.c),
            // 0xb2	ORA D	1	Z, S, P, CY, AC	A <- A | D
            0xB2 => self.op_or(self.d),
            // 0xb3	ORA E	1	Z, S, P, CY, AC	A <- A | E
            0xB3 => self.op_or(self.e),
            // 0xb4	ORA H	1	Z, S, P, CY, AC	A <- A | H
            0xB4 => self.op_or(self.h),
            // 0xb5	ORA L	1	Z, S, P, CY, AC	A <- A | L
            0xB5 => self.op_or(self.l),
            // 0xb6	ORA M	1	Z, S, P, CY, AC	A <- A | (HL)
            0xB6 => self.op_or(self.mem[hl]),
            // 0xb7	ORA A	1	Z, S, P, CY, AC	A <- A | A
            0xB7 => self.op_or(self.a),
            // 0xb8	CMP B	1	Z, S, P, CY, AC	A - B
            // 0xb9	CMP C	1	Z, S, P, CY, AC	A - C
            // 0xba	CMP D	1	Z, S, P, CY, AC	A - D
            // 0xbb	CMP E	1	Z, S, P, CY, AC	A - E
            // 0xbc	CMP H	1	Z, S, P, CY, AC	A - H
            // 0xbd	CMP L	1	Z, S, P, CY, AC	A - L
            // 0xbe	CMP M	1	Z, S, P, CY, AC	A - (HL)
            // 0xbf	CMP A	1	Z, S, P, CY, AC	A - A
            // 0xc0	RNZ	1		if NZ, RET
            0xC0 => {
                if !self.cc.z {
                    self.op_ret();
                }
            }

            // 0xc1	POP B	1		C <- (sp); B <- (sp+1); sp <- sp+2
            0xC1 => {
                self.c = self.mem[self.sp];
                self.b = self.mem[self.sp + 1];
                self.sp += 2;
            }
            // 0xd1	POP D	1		E <- (sp); D <- (sp+1); sp <- sp+2
            0xD1 => {
                self.e = self.mem[self.sp];
                self.d = self.mem[self.sp + 1];
                self.sp += 2;
            }
            // 0xe1	POP H	1		L <- (sp); H <- (sp+1); sp <- sp+2
            0xE1 => {
                self.l = self.mem[self.sp];
                self.h = self.mem[self.sp + 1];
                self.sp += 2;
            }
            // 0xf1	POP PSW	1		flags <- (sp); A <- (sp+1); sp <- sp+2
            0xF1 => {
                self.cc.decode_u8(self.mem[self.sp]);
                self.a = self.mem[self.sp + 1];
                self.sp += 2;
            }

            // 0xc5	PUSH B	1		(sp-2)<-C; (sp-1)<-B; sp <- sp - 2
            0xC5 => {
                self.mem[self.sp - 1] = self.b;
                self.mem[self.sp - 2] = self.c;
                self.sp -= 2;
            }
            // 0xd5	PUSH D	1		(sp-2)<-E; (sp-1)<-D; sp <- sp - 2
            0xD5 => {
                self.mem[self.sp - 1] = self.d;
                self.mem[self.sp - 2] = self.e;
                self.sp -= 2;
            }
            // 0xe5	PUSH H	1		(sp-2)<-L; (sp-1)<-H; sp <- sp - 2
            0xE5 => {
                self.mem[self.sp - 1] = self.h;
                self.mem[self.sp - 2] = self.l;
                self.sp -= 2;
            }
            // 0xf5	PUSH PSW	1		(sp-2)<-flags; (sp-1)<-A; sp <- sp - 2
            0xF5 => {
                self.mem[self.sp - 1] = self.a;
                self.mem[self.sp - 2] = self.cc.encode_u8();
                self.sp -= 2;
            }

            // 0xc2	JNZ adr	3		if NZ, PC <- adr
            0xC2 => {
                if !self.cc.z {
                    let addr = self.next_word();
                    self.op_jump(addr);
                }
            }
            // 0xc3	JMP adr	3		PC <= adr
            0xC3 => {
                let addr = self.next_word();
                self.op_jump(addr);
            }
            // 0xc4	CNZ adr	3		if NZ, CALL adr
            0xC4 => {
                if !self.cc.z {
                    let addr = self.next_word();
                    self.op_call(addr);
                }
            }
            // 0xc6	ADI D8	2	Z, S, P, CY, AC	A <- A + byte
            // 0xc7	RST 0	1		CALL $0
            // 0xc8	RZ	1		if Z, RET
            0xC8 => {
                if self.cc.z {
                    self.op_ret();
                }
            }
            // 0xc9	RET	1		PC.lo <- (sp); PC.hi<-(sp+1); SP <- SP+2
            // 0xca	JZ adr	3		if Z, PC <- adr
            0xCA => {
                if self.cc.z {
                    self.op_jump(hl);
                }
            }
            // 0xcb	-
            // 0xcc	CZ adr	3		if Z, CALL adr
            0xCC => {
                if self.cc.z {
                    self.op_call(hl);
                }
            }
            // 0xcd	CALL adr	3		(SP-1)<-PC.hi;(SP-2)<-PC.lo;SP<-SP-2;PC=adr
            0xCD => {
                let addr = self.next_word();
                self.op_call(addr);
            }
            // 0xce	ACI D8	2	Z, S, P, CY, AC	A <- A + data + CY
            // 0xcf	RST 1	1		CALL $8
            // 0xd0	RNC	1		if NCY, RET
            0xD0 => {
                if !self.cc.cy {
                    self.op_ret();
                }
            }
            // 0xd2	JNC adr	3		if NCY, PC<-adr
            0xD2 => {
                if !self.cc.cy {
                    self.op_jump(hl);
                }
            }
            // 0xd3	OUT D8	2		special
            0xD3 => {}
            // 0xd4	CNC adr	3		if NCY, CALL adr
            0xD4 => {
                if !self.cc.cy {
                    self.op_call(hl);
                }
            }
            // 0xd6	SUI D8	2	Z, S, P, CY, AC	A <- A - data
            // 0xd7	RST 2	1		CALL $10
            // 0xd8	RC	1		if CY, RET
            // 0xd9	-
            // 0xda	JC adr	3		if CY, PC<-adr
            0xDA => {
                if self.cc.cy {
                    self.op_jump(hl);
                }
            }
            // 0xdb	IN D8	2		special
            0xDB => {}
            // 0xdc	CC adr	3		if CY, CALL adr
            0xDC => {
                if self.cc.cy {
                    self.op_call(hl);
                }
            }
            // 0xdd	-
            // 0xde	SBI D8	2	Z, S, P, CY, AC	A <- A - data - CY
            // 0xdf	RST 3	1		CALL $18
            // 0xe0	RPO	1		if PO, RET
            0xE0 => {
                if self.cc.p {
                    self.op_ret()
                }
            }
            // 0xe2	JPO adr	3		if PO, PC <- adr
            0xE2 => {
                if self.cc.p {
                    self.op_jump(hl);
                }
            }
            // 0xe3	XTHL	1		L <-> (SP); H <-> (SP+1)
            // 0xe4	CPO adr	3		if PO, CALL adr
            0xE4 => {
                if self.cc.p {
                    self.op_call(hl);
                }
            }
            // 0xe6	ANI D8	2	Z, S, P, CY, AC	A <- A & data
            // 0xe7	RST 4	1		CALL $20
            // 0xe8	RPE	1		if PE, RET
            0xE8 => {
                if !self.cc.p {
                    self.op_ret()
                }
            }
            // 0xe9	PCHL	1		PC.hi <- H; PC.lo <- L
            // 0xea	JPE adr	3		if PE, PC <- adr
            0xEA => {
                if !self.cc.p {
                    self.op_jump(hl);
                }
            }
            // 0xeb	XCHG	1		H <-> D; L <-> E
            // 0xec	CPE adr	3		if PE, CALL adr
            0xEC => {
                if !self.cc.p {
                    self.op_call(hl);
                }
            }
            // 0xed	-
            // 0xee	XRI D8	2	Z, S, P, CY, AC	A <- A ^ data
            // 0xef	RST 5	1		CALL $28
            // 0xf0	RP	1		if P, RET
            0xF0 => {
                if self.cc.p {
                    self.op_ret()
                }
            }
            // 0xf2	JP adr	3		if P=1 PC <- adr
            0xF2 => {
                if self.cc.s {
                    self.op_jump(hl);
                }
            }
            // 0xf3	DI	1		special
            0xF3 => {
                self.inte = false;
            }
            // 0xf4	CP adr	3		if P, PC <- adr
            // 0xf6	ORI D8	2	Z, S, P, CY, AC	A <- A | data
            // 0xf7	RST 6	1		CALL $30
            // 0xf8	RM	1		if M, RET
            // 0xf9	SPHL	1		SP=HL
            // 0xfa	JM adr	3		if M, PC <- adr
            0xFA => {
                if !self.cc.s {
                    self.op_jump(hl);
                }
            }
            // 0xfb	EI	1		special
            0xFB => {
                self.inte = true;
            }
            // 0xfc	CM adr	3		if M, CALL adr
            // 0xfd	-
            // 0xfe	CPI D8	2	Z, S, P, CY, AC	A - data
            // 0xff	RST 7	1		CALL $38
            _ => {
                panic!("Invalid OP: {:02X}", op);
            }
        }
    }

    fn update_cc(&mut self, num: u8) {
        self.cc.z = num == 0;
        self.cc.s = num >> 7 == 1;
        self.cc.p = num & 1 == 1;
    }

    fn update_cc_cy(&mut self, num: u8, cy: bool) {
        self.update_cc(num);
        self.cc.cy = cy;
    }

    fn op_add(&mut self, rhs: u8) {
        let (sum, overflow) = self.a.overflowing_add(rhs);

        self.a = sum;
        self.update_cc_cy(sum, overflow);
    }

    fn op_sub(&mut self, rhs: u8) {
        let (res, overflow) = self.a.overflowing_sub(rhs);

        self.a = res;
        self.update_cc_cy(res, overflow);
    }

    fn op_and(&mut self, rhs: u8) {
        let res = self.a & rhs;
        self.a = res;
        self.update_cc_cy(res, false);
    }

    fn op_or(&mut self, rhs: u8) {
        let res = self.a | rhs;
        self.a = res;
        self.update_cc_cy(res, false);
    }

    fn op_xor(&mut self, rhs: u8) {
        let res = self.a ^ rhs;
        self.a = res;
        self.update_cc_cy(res, false);
    }

    fn op_jump(&mut self, addr: u16) {
        self.pc = addr;
    }

    fn op_call(&mut self, addr: u16) {
        self.mem[self.sp - 1] = self.pc as u8;
        self.mem[self.sp - 2] = (self.pc >> 2) as u8;
        self.pc = addr;
        self.sp -= 2;
    }

    fn op_ret(&mut self) {
        self.pc = ((self.mem[self.sp + 1] as u16) << 2) | self.mem[self.sp + 2] as u16;
        self.sp += 2
    }
}
