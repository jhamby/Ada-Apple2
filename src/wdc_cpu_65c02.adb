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

with MOS_CPU_6502.Shared; use MOS_CPU_6502.Shared;

package body WDC_CPU_65C02 with
  SPARK_Mode
is

   ---------------------------
   -- CPU_Execute_WDC_65C02 --
   ---------------------------

   procedure CPU_Execute_WDC_65C02
     (C : in out CPU_6502_Series; Mem : not null access RAM_All_Banks;
      Num_Columns, Num_Lines :        Unsigned_16)
   is
      A, X, Y, SP, Opcode : Unsigned_8;
      P                   : CPU_6502_P;
      PC                  : Unsigned_16;
      Address             : Unsigned_16;
      Cycles_Left         : Integer;
      Cycles              : Unsigned_8;
      Distance, Unused    : Unsigned_8;

      Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD : Boolean;

   begin
      A  := Unsigned_8 (C.A and 16#FF#);
      X  := Unsigned_8 (C.X and 16#FF#);
      Y  := Unsigned_8 (C.Y and 16#FF#);
      P  := C.P;
      SP := Unsigned_8 (C.SP and 16#FF#);
      PC := C.PC;

      Load_Flags (P, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD);

      --  number of cycles to execute to finish Num_Lines scan lines
      Cycles_Left := Integer ((Num_Columns * Num_Lines) - C.Column_Cycle);

      while Cycles_Left > 0 loop
         Mem_IO_Read (C, Mem, PC, Opcode);
         PC := PC + 1;

         case Opcode is
            when 16#00# =>
               Save_Flags (P, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD);
               Op_BRK (C, Mem, P, PC, SP);
               Flag_BCD := False;
               Cycles   := 7;

            when 16#01# =>
               Mode_INDX (C, Mem, PC, Address, X);
               Op_ORA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 6;

            when 16#02# | 16#22# | 16#42# | 16#62# | 16#82# | 16#C2#
              | 16#E2# =>
               --  two-cycle, two-byte NOP
               PC     := PC + 1;
               Cycles := 2;

            when 16#03# | 16#07# | 16#0B# | 16#0F# | 16#13# | 16#17# | 16#1B#
              | 16#1F# | 16#23# | 16#27# | 16#2B# | 16#2F# | 16#33# | 16#37#
              | 16#3B# | 16#3F# | 16#43# | 16#47# | 16#4B# | 16#4F# | 16#53#
              | 16#57# | 16#5B# | 16#5F# | 16#63# | 16#67# | 16#6B# | 16#6F#
              | 16#73# | 16#77# | 16#7B# | 16#7F# | 16#83# | 16#87# | 16#8B#
              | 16#8F# | 16#93# | 16#97# | 16#9B# | 16#9F# | 16#A3# | 16#A7#
              | 16#AB# | 16#AF# | 16#B3# | 16#B7# | 16#BB# | 16#BF# | 16#C3#
              | 16#C7# | 16#CB# | 16#CF# | 16#D3# | 16#D7# | 16#DB# | 16#DF#
              | 16#E3# | 16#E7# | 16#EB# | 16#EF# | 16#F3# | 16#F7# | 16#FB#
              | 16#FF# =>
               --  one-cycle NOP
               Cycles := 1;

            when 16#04# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_TSB (C, Mem, Address, A, Flag_Z);
               Cycles := 5;

            when 16#05# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_ORA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 3;

            when 16#06# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_ASL (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 5;

               --  when 16#07# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_RMB (C, Mem, Address, 16#01#);
               --     Cycles := 5;

            when 16#08# =>
               Save_Flags (P, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD);
               Op_PHP (C, Mem, SP, P);
               Cycles := 3;

            when 16#09# =>
               Mode_IMM (PC, Address);
               Op_ORA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#0A# =>
               Op_ASLA (A, Flag_C, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#0C# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_TSB (C, Mem, Address, A, Flag_Z);
               Cycles := 6;

            when 16#0D# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_ORA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#0E# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_ASL (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 6;

               --  when 16#0F# =>
               --     Op_BBR (C, Mem, PC, 16#01#);
               --     Cycles := 5;

            when 16#10# =>
               Mode_REL (C, Mem, PC, Distance);
               Op_BPL (PC, Distance, Flag_N, Cycles);
               Cycles := Cycles + 2;

            when 16#11# =>
               Mode_INDY (C, Mem, PC, Address, Y, Cycles);
               Op_ORA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 5;

            when 16#12# =>
               Mode_IZPG (C, Mem, PC, Address);
               Op_ORA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 5;

            when 16#14# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_TRB (C, Mem, Address, A, Flag_Z);
               Cycles := 5;

            when 16#15# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_ORA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#16# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_ASL (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 6;

               --  when 16#17# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_RMB (C, Mem, Address, 16#02#);
               --     Cycles := 5;

            when 16#18# =>
               Op_CLC (Flag_C);
               Cycles := 2;

            when 16#19# =>
               Mode_ABSY (C, Mem, PC, Address, Y, Cycles);
               Op_ORA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

            when 16#1A# =>
               Op_INA (A, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#1C# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_TRB (C, Mem, Address, A, Flag_Z);
               Cycles := 6;

            when 16#1D# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_ORA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

            when 16#1E# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_ASL (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := Cycles + 6;

               --  when 16#1F# =>
               --     Op_BBR (C, Mem, PC, 16#02#);
               --     Cycles := 5;

            when 16#20# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_JSR (C, Mem, PC, SP, Address);
               Cycles := 6;

            when 16#21# =>
               Mode_INDX (C, Mem, PC, Address, X);
               Op_AND (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 6;

            when 16#24# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_BIT (C, Mem, Address, A, Flag_N, Flag_V, Flag_Z);
               Cycles := 3;

            when 16#25# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_AND (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 3;

            when 16#26# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_ROL (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 5;

               --  when 16#27# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_RMB (C, Mem, Address, 16#04#);
               --     Cycles := 5;

            when 16#28# =>
               Op_PLP (C, Mem, SP, P);
               Load_Flags (P, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD);
               Cycles := 4;

            when 16#29# =>
               Mode_IMM (PC, Address);
               Op_AND (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#2A# =>
               Op_ROLA (A, Flag_C, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#2C# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_BIT (C, Mem, Address, A, Flag_N, Flag_V, Flag_Z);
               Cycles := 4;

            when 16#2D# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_AND (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#2E# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_ROL (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 6;

               --  when 16#2F# =>
               --     Op_BBR (C, Mem, PC, 16#04#);
               --     Cycles := 5;

            when 16#30# =>
               Mode_REL (C, Mem, PC, Distance);
               Op_BMI (PC, Distance, Flag_N, Cycles);
               Cycles := Cycles + 2;

            when 16#31# =>
               Mode_INDY (C, Mem, PC, Address, Y, Cycles);
               Op_AND (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 5;

            when 16#32# =>
               Mode_IZPG (C, Mem, PC, Address);
               Op_AND (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 5;

            when 16#34# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_BIT (C, Mem, Address, A, Flag_N, Flag_V, Flag_Z);
               Cycles := 4;

            when 16#35# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_AND (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#36# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_ROL (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 6;

               --  when 16#37# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_RMB (C, Mem, Address, 16#08#);
               --     Cycles := 5;

            when 16#38# =>
               Op_SEC (Flag_C);
               Cycles := 2;

            when 16#39# =>
               Mode_ABSY (C, Mem, PC, Address, Y, Cycles);
               Op_AND (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

            when 16#3A# =>
               Op_DEA (A, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#3C# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_BIT (C, Mem, Address, A, Flag_N, Flag_V, Flag_Z);
               Cycles := Cycles + 4;

            when 16#3D# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_AND (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

            when 16#3E# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_ROL (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := Cycles + 6;

               --  when 16#3F# =>
               --     Op_BBR (C, Mem, PC, 16#08#);
               --     Cycles := 5;

            when 16#40# =>
               Op_RTI (C, Mem, SP, P, PC);
               Load_Flags (P, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD);
               Cycles := 6;

            when 16#41# =>
               Mode_INDX (C, Mem, PC, Address, X);
               Op_EOR (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 6;

            when 16#44# =>
               Mode_ZPG (C, Mem, PC, Address);
               Mem_IO_Read (C, Mem, Address, Unused);
               --  NOP (read ignored)
               Cycles := 3;

            when 16#45# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_EOR (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 3;

            when 16#46# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_LSR (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 5;

               --  when 16#47# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_RMB (C, Mem, Address, 16#10#);
               --     Cycles := 5;

            when 16#48# =>
               Op_PHA (C, Mem, SP, A);
               Cycles := 3;

            when 16#49# =>
               Mode_IMM (PC, Address);
               Op_EOR (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#4A# =>
               Op_LSRA (A, Flag_C, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#4C# =>
               Get_Word (C, Mem, PC, Address);
               Op_JMP (PC, Address);
               Cycles := 3;

            when 16#4D# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_EOR (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#4E# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_LSR (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 6;

               --  when 16#4F# =>
               --     Op_BBR (C, Mem, PC, 16#10#);
               --     Cycles := 5;

            when 16#50# =>
               Mode_REL (C, Mem, PC, Distance);
               Op_BVC (PC, Distance, Flag_V, Cycles);
               Cycles := Cycles + 2;

            when 16#51# =>
               Mode_INDY (C, Mem, PC, Address, Y, Cycles);
               Op_EOR (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 5;

            when 16#52# =>
               Mode_IZPG (C, Mem, PC, Address);
               Op_EOR (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 5;

            when 16#54# | 16#D4# | 16#F4# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Mem_IO_Read (C, Mem, Address, Unused);
               --  NOP (read ignored)
               Cycles := 4;

            when 16#55# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_EOR (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#56# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_LSR (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 6;

               --  when 16#57# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_RMB (C, Mem, Address, 16#20#);
               --     Cycles := 5;

            when 16#58# =>
               Op_CLI (P);
               Cycles := 2;

            when 16#59# =>
               Mode_ABSY (C, Mem, PC, Address, Y, Cycles);
               Op_EOR (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

            when 16#5A# =>
               Op_PHY (C, Mem, SP, Y);
               Cycles := 3;

            when 16#5C# =>
               Mode_ABSX (C, Mem, PC, Address, X);
               Mem_IO_Read (C, Mem, Address, Unused);
               --  NOP (reads from somewhere in 64K range)
               Cycles := 8;

            when 16#5D# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_EOR (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

            when 16#5E# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_LSR (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := Cycles + 6;

               --  when 16#5F# =>
               --     Op_BBR (C, Mem, PC, 16#20#);
               --     Cycles := 5;

            when 16#60# =>
               Op_RTS (C, Mem, SP, PC);
               Cycles := 6;

            when 16#61# =>
               Mode_INDX (C, Mem, PC, Address, X);
               Cycles := 0;
               Op_ADC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 6;

            when 16#64# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_STZ (C, Mem, Address);
               Cycles := 3;

            when 16#65# =>
               Mode_ZPG (C, Mem, PC, Address);
               Cycles := 0;
               Op_ADC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 3;

            when 16#66# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_ROR (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 5;

               --  when 16#67# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_RMB (C, Mem, Address, 16#40#);
               --     Cycles := 5;

            when 16#68# =>
               Op_PLA (C, Mem, SP, A, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#69# =>
               Mode_IMM (PC, Address);
               Cycles := 0;
               Op_ADC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 2;

            when 16#6A# =>
               Op_RORA (A, Flag_C, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#6C# =>
               Mode_IABSCMOS (C, Mem, PC, Address, Cycles);
               Op_JMP (PC, Address);
               Cycles := Cycles + 6;

            when 16#6D# =>
               Mode_ABS (C, Mem, PC, Address);
               Cycles := 0;
               Op_ADC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 4;

            when 16#6E# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_ROR (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 6;

               --  when 16#6F# =>
               --     Op_BBR (C, Mem, PC, 16#40#);
               --     Cycles := 5;

            when 16#70# =>
               Mode_REL (C, Mem, PC, Distance);
               Op_BVS (PC, Distance, Flag_V, Cycles);
               Cycles := Cycles + 2;

            when 16#71# =>
               Mode_INDY (C, Mem, PC, Address, Y, Cycles);
               Op_ADC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 5;

            when 16#72# =>
               Mode_IZPG (C, Mem, PC, Address);
               Cycles := 0;
               Op_ADC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 5;

            when 16#74# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_STZ (C, Mem, Address);
               Cycles := 4;

            when 16#75# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Cycles := 0;
               Op_ADC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 4;

            when 16#76# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_ROR (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := 6;

               --  when 16#77# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_RMB (C, Mem, Address, 16#80#);
               --     Cycles := 5;

            when 16#78# =>
               Op_SEI (P);
               Cycles := 2;

            when 16#79# =>
               Mode_ABSY (C, Mem, PC, Address, Y, Cycles);
               Op_ADC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 4;

            when 16#7A# =>
               Op_PLY (C, Mem, SP, Y, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#7C# =>
               Mode_IABSX (C, Mem, PC, Address, X);
               Op_JMP (PC, Address);
               Cycles := 6;

            when 16#7D# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_ADC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 4;

            when 16#7E# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_ROR (C, Mem, Address, Flag_C, Flag_N, Flag_Z);
               Cycles := Cycles + 6;

               --  when 16#7F# =>
               --     Op_BBR (C, Mem, PC, 16#80#);
               --     Cycles := 5;

            when 16#80# =>
               Mode_REL (C, Mem, PC, Distance);
               Op_BRA (PC, Distance, Cycles);
               Cycles := Cycles + 2;

            when 16#81# =>
               Mode_INDX (C, Mem, PC, Address, X);
               Op_STA (C, Mem, Address, A);
               Cycles := 6;

            when 16#84# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_STY (C, Mem, Address, Y);
               Cycles := 3;

            when 16#85# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_STA (C, Mem, Address, A);
               Cycles := 3;

            when 16#86# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_STX (C, Mem, Address, X);
               Cycles := 3;

               --  when 16#87# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_SMB (C, Mem, Address, 16#01#);
               --     Cycles := 5;

            when 16#88# =>
               Op_DEY (Y, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#89# =>
               Mode_IMM (PC, Address);
               Op_BITI (C, Mem, Address, A, Flag_Z);
               Cycles := 2;

            when 16#8A# =>
               Op_TXA (X, A, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#8C# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_STY (C, Mem, Address, Y);
               Cycles := 4;

            when 16#8D# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_STA (C, Mem, Address, A);
               Cycles := 4;

            when 16#8E# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_STX (C, Mem, Address, X);
               Cycles := 4;

               --  when 16#8F# =>
               --     Op_BBS (C, Mem, PC, 16#01#);
               --     Cycles := 5;

            when 16#90# =>
               Mode_REL (C, Mem, PC, Distance);
               Op_BCC (PC, Distance, Flag_C, Cycles);
               Cycles := Cycles + 2;

            when 16#91# =>
               Mode_INDY (C, Mem, PC, Address, Y);
               Op_STA (C, Mem, Address, A);
               Cycles := 6;

            when 16#92# =>
               Mode_IZPG (C, Mem, PC, Address);
               Op_STA (C, Mem, Address, A);
               Cycles := 5;

            when 16#94# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_STY (C, Mem, Address, Y);
               Cycles := 4;

            when 16#95# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_STA (C, Mem, Address, A);
               Cycles := 4;

            when 16#96# =>
               Mode_ZPGY (C, Mem, PC, Address, Y);
               Op_STX (C, Mem, Address, X);
               Cycles := 4;

               --  when 16#97# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_SMB (C, Mem, Address, 16#02#);
               --     Cycles := 5;

            when 16#98# =>
               Op_TYA (Y, A, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#99# =>
               Mode_ABSY (C, Mem, PC, Address, Y);
               Op_STA (C, Mem, Address, A);
               Cycles := 5;

            when 16#9A# =>
               Op_TXS (X, SP);
               Cycles := 2;

            when 16#9C# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_STZ (C, Mem, Address);
               Cycles := 4;

            when 16#9D# =>
               Mode_ABSX (C, Mem, PC, Address, X);
               Op_STA (C, Mem, Address, A);
               Cycles := 5;

            when 16#9E# =>
               Mode_ABSX (C, Mem, PC, Address, X);
               Op_STZ (C, Mem, Address);
               Cycles := 5;

               --  when 16#9F# =>
               --     Op_BBS (C, Mem, PC, 16#02#);
               --     Cycles := 5;

            when 16#A0# =>
               Mode_IMM (PC, Address);
               Op_LDY (C, Mem, Address, Y, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#A1# =>
               Mode_INDX (C, Mem, PC, Address, X);
               Op_LDA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 6;

            when 16#A2# =>
               Mode_IMM (PC, Address);
               Op_LDX (C, Mem, Address, X, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#A4# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_LDY (C, Mem, Address, Y, Flag_N, Flag_Z);
               Cycles := 3;

            when 16#A5# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_LDA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 3;

            when 16#A6# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_LDX (C, Mem, Address, X, Flag_N, Flag_Z);
               Cycles := 3;

               --  when 16#A7# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_SMB (C, Mem, Address, 16#04#);
               --     Cycles := 5;

            when 16#A8# =>
               Op_TAY (A, Y, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#A9# =>
               Mode_IMM (PC, Address);
               Op_LDA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#AA# =>
               Op_TAX (A, X, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#AC# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_LDY (C, Mem, Address, Y, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#AD# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_LDA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#AE# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_LDX (C, Mem, Address, X, Flag_N, Flag_Z);
               Cycles := 4;

               --  when 16#AF# =>
               --     Op_BBS (C, Mem, PC, 16#04#);
               --     Cycles := 5;

            when 16#B0# =>
               Mode_REL (C, Mem, PC, Distance);
               Op_BCS (PC, Distance, Flag_C, Cycles);
               Cycles := Cycles + 2;

            when 16#B1# =>
               Mode_INDY (C, Mem, PC, Address, Y, Cycles);
               Op_LDA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 5;

            when 16#B2# =>
               Mode_IZPG (C, Mem, PC, Address);
               Op_LDA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 5;

            when 16#B4# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_LDY (C, Mem, Address, Y, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#B5# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_LDA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#B6# =>
               Mode_ZPGY (C, Mem, PC, Address, Y);
               Op_LDX (C, Mem, Address, X, Flag_N, Flag_Z);
               Cycles := 4;

               --  when 16#B7# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_SMB (C, Mem, Address, 16#08#);
               --     Cycles := 5;

            when 16#B8# =>
               Op_CLV (Flag_V);
               Cycles := 2;

            when 16#B9# =>
               Mode_ABSY (C, Mem, PC, Address, Y, Cycles);
               Op_LDA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

            when 16#BA# =>
               Op_TSX (SP, X, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#BC# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_LDY (C, Mem, Address, Y, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

            when 16#BD# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_LDA (C, Mem, Address, A, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

            when 16#BE# =>
               Mode_ABSY (C, Mem, PC, Address, Y, Cycles);
               Op_LDX (C, Mem, Address, X, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

               --  when 16#BF# =>
               --     Op_BBS (C, Mem, PC, 16#08#);
               --     Cycles := 5;

            when 16#C0# =>
               Mode_IMM (PC, Address);
               Op_CPY (C, Mem, Address, Y, Flag_C, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#C1# =>
               Mode_INDX (C, Mem, PC, Address, X);
               Op_CMP (C, Mem, Address, A, Flag_C, Flag_N, Flag_Z);
               Cycles := 6;

            when 16#C4# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_CPY (C, Mem, Address, Y, Flag_C, Flag_N, Flag_Z);
               Cycles := 3;

            when 16#C5# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_CMP (C, Mem, Address, A, Flag_C, Flag_N, Flag_Z);
               Cycles := 3;

            when 16#C6# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_DEC (C, Mem, Address, Flag_N, Flag_Z);
               Cycles := 5;

               --  when 16#C7# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_SMB (C, Mem, Address, 16#10#);
               --     Cycles := 5;

            when 16#C8# =>
               Op_INY (Y, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#C9# =>
               Mode_IMM (PC, Address);
               Op_CMP (C, Mem, Address, A, Flag_C, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#CA# =>
               Op_DEX (X, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#CC# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_CPY (C, Mem, Address, Y, Flag_C, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#CD# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_CMP (C, Mem, Address, A, Flag_C, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#CE# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_DEC (C, Mem, Address, Flag_N, Flag_Z);
               Cycles := 5;

               --  when 16#CF# =>
               --     Op_BBS (C, Mem, PC, 16#10#);
               --     Cycles := 5;

            when 16#D0# =>
               Mode_REL (C, Mem, PC, Distance);
               Op_BNE (PC, Distance, Flag_Z, Cycles);
               Cycles := Cycles + 2;

            when 16#D1# =>
               Mode_INDY (C, Mem, PC, Address, Y, Cycles);
               Op_CMP (C, Mem, Address, A, Flag_C, Flag_N, Flag_Z);
               Cycles := Cycles + 5;

            when 16#D2# =>
               Mode_IZPG (C, Mem, PC, Address);
               Op_CMP (C, Mem, Address, A, Flag_C, Flag_N, Flag_Z);
               Cycles := 5;

            when 16#D5# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_CMP (C, Mem, Address, A, Flag_C, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#D6# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_DEC (C, Mem, Address, Flag_N, Flag_Z);
               Cycles := 6;

               --  when 16#D7# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_SMB (C, Mem, Address, 16#20#);
               --     Cycles := 5;

            when 16#D8# =>
               Op_CLD (Flag_BCD);
               Cycles := 2;

            when 16#D9# =>
               Mode_ABSY (C, Mem, PC, Address, Y, Cycles);
               Op_CMP (C, Mem, Address, A, Flag_C, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

            when 16#DA# =>
               Op_PHX (C, Mem, SP, X);
               Cycles := 3;

            when 16#DC# | 16#FC# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Mem_IO_Read (C, Mem, Address, Unused);
               --  NOP (read ignored)
               Cycles := Cycles + 4;

            when 16#DD# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_CMP (C, Mem, Address, A, Flag_C, Flag_N, Flag_Z);
               Cycles := Cycles + 4;

            when 16#DE# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_DEC (C, Mem, Address, Flag_N, Flag_Z);
               Cycles := Cycles + 6;

               --  when 16#DF# =>
               --     Op_BBS (C, Mem, PC, 16#20#);
               --     Cycles := 5;

            when 16#E0# =>
               Mode_IMM (PC, Address);
               Op_CPX (C, Mem, Address, X, Flag_C, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#E1# =>
               Mode_INDX (C, Mem, PC, Address, X);
               Cycles := 0;
               Op_SBC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 6;

            when 16#E4# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_CPX (C, Mem, Address, X, Flag_C, Flag_N, Flag_Z);
               Cycles := 3;

            when 16#E5# =>
               Mode_ZPG (C, Mem, PC, Address);
               Cycles := 0;
               Op_SBC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 3;

            when 16#E6# =>
               Mode_ZPG (C, Mem, PC, Address);
               Op_INC (C, Mem, Address, Flag_N, Flag_Z);
               Cycles := 5;

               --  when 16#E7# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_SMB (C, Mem, Address, 16#40#);
               --     Cycles := 5;

            when 16#E8# =>
               Op_INX (X, Flag_N, Flag_Z);
               Cycles := 2;

            when 16#E9# =>
               Mode_IMM (PC, Address);
               Cycles := 0;
               Op_SBC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 2;

            when 16#EA# =>
               --  two-cycle, one-byte NOP
               Cycles := 2;

            when 16#EC# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_CPX (C, Mem, Address, X, Flag_C, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#ED# =>
               Mode_ABS (C, Mem, PC, Address);
               Cycles := 0;
               Op_SBC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 4;

            when 16#EE# =>
               Mode_ABS (C, Mem, PC, Address);
               Op_INC (C, Mem, Address, Flag_N, Flag_Z);
               Cycles := 6;

               --  when 16#EF# =>
               --     Op_BBS (C, Mem, PC, 16#40#);
               --     Cycles := 5;

            when 16#F0# =>
               Mode_REL (C, Mem, PC, Distance);
               Op_BEQ (PC, Distance, Flag_Z, Cycles);
               Cycles := Cycles + 2;

            when 16#F1# =>
               Mode_INDY (C, Mem, PC, Address, Y, Cycles);
               Op_SBC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 5;

            when 16#F2# =>
               Mode_IZPG (C, Mem, PC, Address);
               Cycles := 0;
               Op_SBC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 5;

            when 16#F5# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Cycles := 0;
               Op_SBC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 4;

            when 16#F6# =>
               Mode_ZPGX (C, Mem, PC, Address, X);
               Op_INC (C, Mem, Address, Flag_N, Flag_Z);
               Cycles := 6;

               --  when 16#F7# =>
               --     Mode_ZPG (C, Mem, PC, Address);
               --     Op_SMB (C, Mem, Address, 16#80#);
               --     Cycles := 5;

            when 16#F8# =>
               Op_SED (Flag_BCD);
               Cycles := 2;

            when 16#F9# =>
               Mode_ABSY (C, Mem, PC, Address, Y, Cycles);
               Op_SBC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 4;

            when 16#FA# =>
               Op_PLX (C, Mem, SP, X, Flag_N, Flag_Z);
               Cycles := 4;

            when 16#FD# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_SBC_CMOS
                 (C, Mem, Address, A, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD,
                  Cycles);
               Cycles := Cycles + 4;

            when 16#FE# =>
               Mode_ABSX (C, Mem, PC, Address, X, Cycles);
               Op_INC (C, Mem, Address, Flag_N, Flag_Z);
               Cycles := Cycles + 6;

               --  when 16#FF# =>
               --     Op_BBS (C, Mem, PC, 16#80#);
               --     Cycles := 5;

         end case;

         Cycles_Left    := Cycles_Left - Integer (Cycles);
         C.Column_Cycle := C.Column_Cycle + Unsigned_16 (Cycles);
         if C.Column_Cycle >= Num_Columns then
            C.Column_Cycle := C.Column_Cycle - Num_Columns;
            C.Scan_Line    := C.Scan_Line + 1;
         end if;
      end loop;

      Save_Flags (P, Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD);
      C.A  := Unsigned_16 (A);
      C.X  := Unsigned_16 (X);
      C.Y  := Unsigned_16 (Y);
      C.P  := P;
      C.SP := Unsigned_16 (SP) or 16#0100#;
      C.PC := PC;
   end CPU_Execute_WDC_65C02;

end WDC_CPU_65C02;
