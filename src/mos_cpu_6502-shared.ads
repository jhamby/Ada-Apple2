--  AppleWin : An Apple //e emulator for Windows
--
--  Copyright (C) 1994-1996, Michael O'Brien
--  Copyright (C) 1999-2001, Oliver Schmidt
--  Copyright (C) 2002-2005, Tom Charlesworth
--  Copyright (C) 2006-2007, Tom Charlesworth, Michael Pohoreski
--  Copyright (C) 2023, Jake Hamby (Ada port)
--
--  AppleWin is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  AppleWin is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with AppleWin; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

package MOS_CPU_6502.Shared with
  SPARK_Mode
is
   -----------------------
   -- Helper procedures --
   -----------------------

   procedure Pop
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; Value : out Unsigned_8) with
     Inline;
   --  Pop value from stack

   procedure Push
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; Value : Unsigned_8) with
     Inline;
   --  Push value to stack

   procedure Load_Flags
     (P                                        :     CPU_6502_P;
      Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD : out Boolean) with
     Inline;
   --  Load flags from P register into booleans

   procedure Save_Flags
     (P                                        : in out CPU_6502_P;
      Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD :        Boolean) with
     Inline;
   --  Save flags from booleans into P register

   procedure Set_NZ (Value : Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;
   --  Set N and Z flags

   procedure Set_Z (Value : Unsigned_8; Flag_Z : out Boolean) with
     Inline;
   --  Set Z flag

   procedure Branch_Taken (PC : in out Unsigned_16; Distance : Unsigned_8) with
     Inline;
   --  update PC with signed branch displacement

   procedure Branch_Taken
     (PC           : in out Unsigned_16; Distance : Unsigned_8;
      Extra_Cycles :    out Unsigned_8) with
     Inline;
   --  update PC and add extra cycle(s) for branch taken and page crossing

   procedure Check_Page_Change
     (Base, PC : Unsigned_16; Extra_Cycles : out Unsigned_8) with
     Inline;
   --  Only call for opcodes that are slower on page cross

   procedure Get_Word
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Word : out Unsigned_16) with
     Inline;

   procedure Get_Word_FF_Wrap
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Word : out Unsigned_16) with
     Inline;
   --  Get word with $xxFF wraparound behavior

   ----------------------
   -- Addressing modes --
   ----------------------

   procedure Mode_ABS
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16) with
     Inline;

   procedure Mode_IABSX
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; X : Unsigned_8) with
     Inline;

   procedure Mode_ABSX
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; X : Unsigned_8) with
     Inline;

   procedure Mode_ABSX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; X : Unsigned_8;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Mode_ABSY
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; Y : Unsigned_8) with
     Inline;

   procedure Mode_ABSY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; Y : Unsigned_8;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Mode_IABSCMOS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC           : in out Unsigned_16; Address : out Unsigned_16;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Mode_IABSNMOS
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16) with
     Inline;

   procedure Mode_IMM (PC : in out Unsigned_16; Address : out Unsigned_16) with
     Inline;

   procedure Mode_INDX
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; X : Unsigned_8) with
     Inline;

   procedure Mode_INDY
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; Y : Unsigned_8) with
     Inline;

   procedure Mode_INDY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; Y : Unsigned_8;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Mode_IZPG
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16) with
     Inline;

   procedure Mode_REL
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Distance : out Unsigned_8) with
     Inline;

   procedure Mode_ZPG
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16) with
     Inline;

   procedure Mode_ZPGX
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; X : Unsigned_8) with
     Inline;

   procedure Mode_ZPGY
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; Y : Unsigned_8) with
     Inline;

   ------------------
   -- Instructions --
   ------------------

   procedure Op_ADC_NMOS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean) with
     Inline;

   procedure Op_ADC_CMOS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean;
      Extra_Cycles           : in out Unsigned_8) with
     Inline;
   --  note: initialize Extra_Cycles before calling

   procedure Op_ALR
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : in out Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_AND
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; A : in out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_ANC
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : in out Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_ARR
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean) with
     Inline;

   procedure Op_ASL
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Flag_C, Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_ASLA
     (A : in out Unsigned_8; Flag_C, Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_ASO
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : in out Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_AXA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A, X, Base_High_Plus_1 : Unsigned_8) with
     Inline;
   --  Base_High_Plus_1 is the high byte of address before indexing, plus 1

   procedure Op_AXS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A, X : Unsigned_8) with
     Inline;

   procedure Op_BBR
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Test_Bit : Unsigned_8) with
     Inline;

   procedure Op_BBS
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Test_Bit : Unsigned_8) with
     Inline;

   procedure Op_BCC
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_C : Boolean;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Op_BCS
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_C : Boolean;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Op_BEQ
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_Z : Boolean;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Op_BIT
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : Unsigned_8;
      Flag_N, Flag_V, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_BITI
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A : Unsigned_8; Flag_Z : out Boolean) with
     Inline;

   procedure Op_BMI
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_N : Boolean;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Op_BNE
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_Z : Boolean;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Op_BPL
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_N : Boolean;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Op_BRA
     (PC           : in out Unsigned_16; Distance : Unsigned_8;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Op_BRK
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      P  : in out CPU_6502_P; PC : in out Unsigned_16;
      SP : in out Unsigned_8) with
     Inline;
   --  Note: copy local flags to P before calling this procedure

   procedure Op_BVC
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_V : Boolean;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Op_BVS
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_V : Boolean;
      Extra_Cycles :    out Unsigned_8) with
     Inline;

   procedure Op_CLC (Flag_C : out Boolean) with
     Inline;

   procedure Op_CLD (Flag_BCD : out Boolean) with
     Inline;

   procedure Op_CLI (P : in out CPU_6502_P) with
     Inline;

   procedure Op_CLV (Flag_V : out Boolean) with
     Inline;

   procedure Op_CMP
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_CPX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; X : Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_CPY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; Y : Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_DCM
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_DEA (A : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_DEC
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_DEX (X : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_DEY (Y : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_EOR
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; A : in out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_HLT
     (C      : in out CPU_6502_Series'Class; PC : in out Unsigned_16;
      Opcode :        Unsigned_8) with
     Inline;

   procedure Op_INA (A : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_INC
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_INS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean) with
     Inline;

   procedure Op_INX (X : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_INY (Y : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_JMP (PC : out Unsigned_16; Address : Unsigned_16) with
     Inline;

   procedure Op_JSR
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC      : in out Unsigned_16; SP : in out Unsigned_8;
      Address :        Unsigned_16) with
     Inline;

   procedure Op_LAS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; SP : in out Unsigned_8; A, X : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_LAX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; A, X : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_LDA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; A : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_LDX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; X : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_LDY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; Y : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_LSE
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : in out Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_LSR
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Flag_C, Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_LSRA
     (A : in out Unsigned_8; Flag_C, Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_OAL
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        : Unsigned_16; A : in out Unsigned_8; X : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_ORA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; A : in out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_PHA
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; A : Unsigned_8) with
     Inline;

   procedure Op_PHP
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; P : CPU_6502_P) with
     Inline;

   procedure Op_PHX
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; X : Unsigned_8) with
     Inline;

   procedure Op_PHY
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; Y : Unsigned_8) with
     Inline;

   procedure Op_PLA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP             : in out Unsigned_8; A : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_PLP
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; P : out CPU_6502_P) with
     Inline;

   procedure Op_PLX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP             : in out Unsigned_8; X : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_PLY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP             : in out Unsigned_8; Y : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_RLA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_RMB
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Reset_Bit : Unsigned_8) with
     Inline;

   procedure Op_ROL
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; Flag_C : in out Boolean;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_ROLA
     (A              : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_ROR
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; Flag_C : in out Boolean;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_RORA
     (A              : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_RRA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean) with
     Inline;

   procedure Op_RTI
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; P : out CPU_6502_P; PC : out Unsigned_16) with
     Inline;

   procedure Op_RTS
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; PC : out Unsigned_16) with
     Inline;

   procedure Op_SAX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A : Unsigned_8; X : in out Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_SAY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Y, Base_High_Plus_1 : Unsigned_8) with
     Inline;
   --  Base_High_Plus_1 is the high byte of address before indexing, plus 1

   procedure Op_SBC_NMOS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean) with
     Inline;

   procedure Op_SBC_CMOS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean;
      Extra_Cycles           : in out Unsigned_8) with
     Inline;

   procedure Op_SEC (Flag_C : out Boolean) with
     Inline;

   procedure Op_SED (Flag_BCD : out Boolean) with
     Inline;

   procedure Op_SEI (P : in out CPU_6502_P) with
     Inline;

   procedure Op_SMB
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Set_Bit : Unsigned_8) with
     Inline;

   procedure Op_STA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A : Unsigned_8) with
     Inline;

   procedure Op_STX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; X : Unsigned_8) with
     Inline;

   procedure Op_STY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Y : Unsigned_8) with
     Inline;

   procedure Op_STZ
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16) with
     Inline;

   procedure Op_TAS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A, X, Base_High_Plus_1 : Unsigned_8;
      SP      :    out Unsigned_8) with
     Inline;
   --  Base_High_Plus_1 is the high byte of address before indexing, plus 1

   procedure Op_TAX
     (A : Unsigned_8; X : out Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_TAY
     (A : Unsigned_8; Y : out Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_TRB
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A : Unsigned_8; Flag_Z : out Boolean) with
     Inline;

   procedure Op_TSB
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A : Unsigned_8; Flag_Z : out Boolean) with
     Inline;

   procedure Op_TSX
     (SP : Unsigned_8; X : out Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_TXA
     (X : Unsigned_8; A : out Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_TXS (X : Unsigned_8; SP : out Unsigned_8) with
     Inline;

   procedure Op_TYA
     (Y : Unsigned_8; A : out Unsigned_8; Flag_N, Flag_Z : out Boolean) with
     Inline;

   procedure Op_XAA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; X : Unsigned_8; A : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean) with
     Inline;

   procedure Op_XAS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; X, Base_High_Plus_1 : Unsigned_8) with
     Inline;
   --  Base_High_Plus_1 is the high byte of address before indexing, plus 1

end MOS_CPU_6502.Shared;
