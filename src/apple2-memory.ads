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

with Emu.Memory; use Emu.Memory;

package Apple2.Memory with
  SPARK_Mode
is
   pragma Elaborate_Body;

   Num_Slots : constant := 7;
   --  original Apple II has a slot 0 that isn't emulated

   type Full_Slot_Range is range 0 .. Num_Slots;
   --  slot ID, or 0 for no slot

   subtype Slot_Range is Full_Slot_Range range 1 .. Num_Slots;
   --  select slot 1 .. 7

   type Card_ROM_Array is array (Slot_Range) of Card_ROM_Type;
   --  Array of 256-byte peripheral ROMs

   type Expansion_ROM_Array is array (Slot_Range) of Expansion_ROM_Access;
   --  Array of access to 2K peripheral ROMs

   type IRQ_Source is (IRQ_6522, IRQ_Speech, IRQ_SSC, IRQ_MOUSE);

   type Button_ID is (Button_0, Button_1);
   type Button_State is (Button_Up, Button_Down);

   type Computer is new Apple2_Base with record

      Card_ROMs : Card_ROM_Array := (others => (others => 0));

      Expansion_ROMs : Expansion_ROM_Array := (others => null);

      RAM_Active_Aux_Bank : RAM_Bank_Index := RAM_Bank_Aux_Start;
      --  active aux RAM bank

      IO_Select_Slot : Full_Slot_Range := 0;
      --  Slot of expansion ROM to enable (1 - 7 = slots; 0 = none)

      RAM_Max_Bank_Used : RAM_Bank_Index := RAM_Bank_Aux_Start;
      --  highest RAMWorks III bank read or written to

      High_RAM_Write_Latch_Cycle : CPU_Cycle_Count := 0;
      --  Cycle count of first of two reads to enable Mem_High_RAM_Write

   end record;
   --  Child class containing members only accessed by Apple2.Memory

   Apple_IO_Start : constant Unsigned_16 := 16#C000#;
   --  Start of $Cxxx I/O and ROM region

   Apple_Slot_Start : constant Unsigned_16 := 16#C100#;
   --  each slot has 1 page reserved for it

   Apple_Slot_End : constant Unsigned_16 := 16#C7FF#;
   --  end of slot address range

   Expansion_ROM_Start : constant Unsigned_16 := 16#C800#;
   --  2K expansion ROM start

   Expansion_ROM_End : constant Unsigned_16 := 16#CFFF#;
   --  End of I/O address range

   overriding procedure Mem_IO_Read_Special
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Value : out Unsigned_8);
   --  Read a byte from I/O space or floating bus

   overriding procedure Mem_IO_Write_Special
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Value : Unsigned_8);
   --  Write a byte to I/O space

   procedure Init_Apple2
     (C : in out Computer; Mem : not null access RAM_All_Banks);
   --  Initialize ROMs and clear RAM

   procedure Mem_Update_Paging
     (C : in out Computer; Mem : not null access RAM_All_Banks);
   --  Update read and write paging tables

   procedure Mem_Read_Floating_Bus
     (C     : in out Computer; Mem : not null access constant RAM_All_Banks;
      Value :    out Unsigned_8);
   --  Read floating bus address and advance cycle counter

   procedure Mem_Read_Floating_Bus
     (C        : in out Computer; Mem : not null access constant RAM_All_Banks;
      High_Bit :        Boolean; Value : out Unsigned_8);
   --  Read floating bus address, replacing high bit of result

   procedure Mem_Reset
     (C : in out Computer; Mem : not null access RAM_All_Banks);

end Apple2.Memory;
