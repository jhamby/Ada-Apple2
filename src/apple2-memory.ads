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

package Apple2.Memory with
  SPARK_Mode
is
   pragma Elaborate_Body;

   type Computer is new Apple2_Base with null record;

   Apple_Slot_Size : constant Unsigned_16 := 16#0100#;
   --  1 page = $Cx00 .. $CxFF (slot 1 .. 7)

   Apple_Slot_Begin : constant Unsigned_16 := 16#C100#;
   --  each slot has 1 page reserved for it

   Apple_Slot_End : constant Unsigned_16 := 16#C7FF#;
   --  end of slot address range

   Firmware_Expansion_Size : constant Unsigned_16 := 16#0800#;
   --  8 pages = $C800 .. $CFFF

   Firmware_Expansion_Begin : constant Unsigned_16 := 16#C800#;
   --  [C800,CFFF)

   Firmware_Expansion_End : constant Unsigned_16 := 16#CFFF#;
   --  End of I/O address range

   overriding procedure Mem_IO_Read
     (C           : in out Computer; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural);

   --  Read a byte from memory or I/O space

   overriding procedure Mem_IO_Write
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address : Unsigned_16; Write_Value : Unsigned_8; Cycles_Left : Natural);
   --  Write a byte to memory or I/O space

   procedure Mem_IO_Read_Cxxx
     (C           : in out Computer; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural);
   pragma Inline (Mem_IO_Read_Cxxx);
   --  Read a byte from I/O space ($Cxxx)

   procedure Mem_IO_Write_Cxxx
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address : Unsigned_16; Write_Value : Unsigned_8; Cycles_Left : Natural);
   pragma Inline (Mem_IO_Write_Cxxx);
   --  Write a byte to I/O space ($Cxxx)

   function Mem_Get_Mode (C : Computer) return Mem_Flag_Type;
   pragma Inline (Mem_Get_Mode);
   --  Get current memory mode flags

   procedure Mem_Set_Mode (C : in out Computer; Mode : Mem_Flag_Type);
   pragma Inline (Mem_Set_Mode);
   --  Set current memory mode flags

   procedure Init_Apple2
     (C     : in out Computer; Mem : not null access RAM_All_Banks;
      Model :        Apple_2_Model);
   --  Initialize ROMs and clear RAM

   procedure Mem_Read_Floating_Bus
     (C          : in out Computer; Mem : not null access RAM_All_Banks;
      Read_Value :    out Unsigned_8; Executed_Cycles : Natural);
   --  Read floating bus address and advance cycle counter

   procedure Mem_Read_Floating_Bus
     (C               : in out Computer; Mem : not null access RAM_All_Banks;
      High_Bit        :        Boolean; Read_Value : out Unsigned_8;
      Executed_Cycles :        Natural);
   --  Read floating bus address, replacing high bit of result

   procedure Mem_Reset
     (C : in out Computer; Mem : not null access RAM_All_Banks);

   procedure Mem_Reset_Paging (C : in out Computer);
   --  Reset paging tables based on memory mode.
   --  Called by:
   --    Soft-reset (Ctrl+Reset)
   --    Snapshot_Load_State()

   function Mode_80_Store (C : Computer) return Boolean;
   pragma Inline (Mode_80_Store);
   --  Memory mode is 80 Store (used by Video)

   function Mode_High_RAM_Write (C : Computer) return Boolean;
   pragma Inline (Mode_High_RAM_Write);
   --  Memory mode is high RAM write (used by Mem_IO_Write)

   function Mode_High_RAM_Bank_2 (C : Computer) return Boolean;
   pragma Inline (Mode_High_RAM_Bank_2);
   --  Memory mode is high RAM bank 2 (used by Mem_IO_Read & Write)

   procedure IO_Read_Null
     (C          : in out Computer; Mem : not null access RAM_All_Banks;
      Read_Value :    out Unsigned_8; Cycles_Left : Natural);
   pragma Inline (IO_Read_Null);
   --  Default I/O read procedure (count cycles, read floating bus)

   procedure IO_Write_Null (C : in out Computer; Cycles_Left : Natural);
   pragma Inline (IO_Write_Null);
   --  Default I/O write procedure (count cycles)

end Apple2.Memory;
