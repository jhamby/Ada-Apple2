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

with Emu; use Emu;

with Emu.Memory; use Emu.Memory;

with Interfaces; use Interfaces;

package MOS_CPU_6502 with
  SPARK_Mode
is

   type CPU_6502_P is mod 256;
   --  Processor status register type

   P_Flag_Carry        : constant CPU_6502_P := 16#01#; --  carry flag
   P_Flag_Zero         : constant CPU_6502_P := 16#02#; --  zero flag
   P_Flag_IRQB_Disable : constant CPU_6502_P := 16#04#; --  IRQB disable
   P_Flag_Decimal_Mode : constant CPU_6502_P := 16#08#; --  decimal mode
   P_Flag_BRK_Command  : constant CPU_6502_P := 16#10#; --  flags from BRK
   P_Flag_Reserved     : constant CPU_6502_P := 16#20#; --  always set
   P_Flag_Overflow     : constant CPU_6502_P := 16#40#; --  overflow flag
   P_Flag_Negative     : constant CPU_6502_P := 16#80#; --  negative flag

   P_Flag_CNVZD_Mask : constant CPU_6502_P :=
     not
     (P_Flag_Carry or P_Flag_Negative or P_Flag_Overflow or P_Flag_Zero or
      P_Flag_Decimal_Mode);

   type CPU_6502_Series is abstract tagged record

      A, X, Y : Unsigned_8 := 16#FF#;  -- Accumulator, index X, and index Y

      P : CPU_6502_P := 16#FF#;  -- processor status register

      PC : Unsigned_16 := 16#FFFC#;  -- program counter

      SP : Unsigned_8 := 16#FF#;  -- stack pointer (low byte)

      Halt_Opcode : Unsigned_8 := 0;  -- illegal opcode halt (NMOS 6502 only)

      Column_Cycle : Unsigned_16 := 0;
      --  clock cycles since the start of the current scan line, updated
      --  by the CPU before each bus access

      Scan_Line : Unsigned_16 := 0;
      --  current scan line (will be start of VBL at start & end of frame),
      --  updated by the CPU on scan line change

   end record;
   --  Inherited by computers using an MOS 6502, WDC 65C02, or similar

   procedure CPU_Initialize
     (C : in out CPU_6502_Series; Mem : not null access RAM_All_Banks);
   --  Initialize the CPU state

   procedure CPU_Reset
     (C : in out CPU_6502_Series; Mem : not null access RAM_All_Banks);
   --  Reset the CPU state

   procedure CPU_Execute_MOS_6502
     (C : in out CPU_6502_Series; Mem : not null access RAM_All_Banks;
      Num_Columns, Num_Lines :        Unsigned_16);
   --  Emulate an NMOS 6502 CPU for the specified number of cycles, in
   --  cycles per scan line, times number of scan lines to emulate. This
   --  will either be one line or the entire VBL interval. The minimum
   --  duration is one scan line (Num_Columns clock cycles).

   procedure Mem_IO_Access
     (C        : in out CPU_6502_Series; Mem : not null access RAM_All_Banks;
      Address  :        Unsigned_16; Value : in out Unsigned_8;
      Is_Write :        Boolean) is abstract;
   --  Read or write a byte from RAM, ROM, I/O space, or floating bus

   procedure Mem_IO_Read
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Value : out Unsigned_8) with
     Inline;
   --  Read a byte from RAM, ROM, I/O space, or floating bus

   procedure Mem_IO_Write
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Value : Unsigned_8) with
     Inline;
   --  Write a byte to RAM, ROM, I/O space, or floating bus

end MOS_CPU_6502;
