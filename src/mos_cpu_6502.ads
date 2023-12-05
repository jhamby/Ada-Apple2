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

with Emu;        use Emu;
with Emu.Memory; use Emu.Memory;

with Interfaces; use Interfaces;

package MOS_CPU_6502 with
  SPARK_Mode
is

   type CPU_6502_P_Type is mod 256;
   --  Processor status register type

   CPU_6502_P_Flag_Carry        : constant := 16#01#; --  carry flag
   CPU_6502_P_Flag_Zero         : constant := 16#02#; --  zero flag
   CPU_6502_P_Flag_IRQB_Disable : constant := 16#04#; --  IRQB disable
   CPU_6502_P_Flag_Decimal_Mode : constant := 16#08#; --  decimal mode
   CPU_6502_P_Flag_BRK_Command  : constant := 16#10#; --  flags from BRK
   CPU_6502_P_Flag_Overflow     : constant := 16#40#; --  overflow flag
   CPU_6502_P_Flag_Negative     : constant := 16#80#; --  negative flag

   type CPU_6502_Series is abstract tagged record

      A, X, Y : Unsigned_8 := 16#FF#;  -- Accumulator, index X, and index Y

      P : CPU_6502_P_Type := 16#FF#;  -- processor status register

      PC : Unsigned_16 := 16#FFFC#;  -- program counter

      SP : Unsigned_16 := 16#01FF#;  -- stack pointer

      Jammed : Boolean := False;  -- CPU has jammed (NMOS 6502 only)

   end record;
   --  Inherited by computers using an MOS 6502, WDC 65C02, or similar

   procedure CPU_Initialize (C : in out CPU_6502_Series);
   --  Initialize the CPU state

   procedure CPU_Reset (C : in out CPU_6502_Series);
   --  Reset the CPU state

   procedure CPU_Calc_Cycles
     (C : in out CPU_6502_Series; Cycles_Left : Natural);
   --  Update the CPU cycle counters

   procedure CPU_Execute_MOS_6502
     (C : in out CPU_6502_Series; Mem : not null access RAM_All_Banks;
      Total_Cycles :        Natural);
   --  Emulate an NMOS 6502 CPU for the specified number of cycles

   procedure Mem_IO_Read
     (C : in out CPU_6502_Series; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural) is abstract;
   --  Read a byte from memory or I/O space

   procedure Mem_IO_Write
     (C : in out CPU_6502_Series; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Write_Value : Unsigned_8;
      Cycles_Left :        Natural) is abstract;
   --  Write a byte to memory or I/O space

end MOS_CPU_6502;
