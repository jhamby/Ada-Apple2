pragma SPARK_Mode;

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

package Apple2.Memory is

   type Mem_Flag_Type is mod 2**16;
   --  Memory map behavior flags

   Flag_No_Flags        : constant Mem_Flag_Type := 16#0000#;
   Flag_80_Store        : constant Mem_Flag_Type := 16#0001#;
   Flag_Alt_ZP          : constant Mem_Flag_Type := 16#0002#;
   Flag_Aux_Read        : constant Mem_Flag_Type := 16#0004#;
   Flag_Aux_Write       : constant Mem_Flag_Type := 16#0008#;
   Flag_High_RAM_Bank_2 : constant Mem_Flag_Type := 16#0010#;
   Flag_High_RAM        : constant Mem_Flag_Type := 16#0020#;
   Flag_Hi_Res          : constant Mem_Flag_Type := 16#0040#;
   Flag_Page_2          : constant Mem_Flag_Type := 16#0080#;
   Flag_Slot_C3_ROM     : constant Mem_Flag_Type := 16#0100#;
   Flag_Slot_CX_ROM     : constant Mem_Flag_Type := 16#0200#;
   Flag_High_RAM_Write  : constant Mem_Flag_Type := 16#0400#;

   function "+" (L, R : Mem_Flag_Type) return Mem_Flag_Type;
   --  Combine flag L with flag R

   Apple_Slot_Size  : constant Address_16_Bit := 16#0100#;
   --  1 page = $Cx00 .. $CxFF (slot 1 .. 7)

   Apple_Slot_Begin : constant Address_16_Bit := 16#C100#;
   --  each slot has 1 page reserved for it

   Apple_Slot_End   : constant Address_16_Bit := 16#C7FF#;
   --  end of slot address range

   Firmware_Expansion_Size : constant Address_16_Bit := 16#0800#;
   --  8 pages = $C800 .. $CFFF

   Firmware_Expansion_Begin : constant Address_16_Bit := 16#C800#;
   --  [C800,CFFF)

   Firmware_Expansion_End   : constant Address_16_Bit := 16#CFFF#;
   --  End of I/O address range

   RAM_Works_Num_Pages : constant := 128;
   --  Always use the largest RAMWorks III size (128 x 64K pages = 8 MB)

   type RAM_Works_Bank_Range is range 0 .. RAM_Works_Num_Pages - 1;
   --  RAMWorks III bank number (0 - 127)

   type Mem_Init_Pattern_Type is (Pattern_Zero, Pattern_FF_FF_00_00);

   Mem_Init_Pattern : Mem_Init_Pattern_Type := Pattern_FF_FF_00_00;
   --  Current memory init pattern

   procedure Mem_IO_Read
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) with
     Pre => Address in 16#C000# .. 16#CFFF#;
   --  Read a byte from I/O space

   procedure Mem_IO_Write
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) with
     Pre => Address in 16#C000# .. 16#CFFF#;
   --  Write a byte to I/O space

   procedure Mem_Get_Aux_Range
     (Offset, Length : Address_16_Bit; Mem_Range : out Mem_Range_Access);
   --  Get an access pointer to the requested range of aux. memory

   procedure Mem_Get_Main_Range
     (Offset, Length : Address_16_Bit; Mem_Range : out Mem_Range_Access);
   --  Get an access pointer to the requested range of main memory

   procedure Mem_Get_Cx_ROM_Peripheral (Mem_Range : out Mem_Range_Access);
   --  Get an access pointer to the Cx ROM range

   function Mem_Get_Mode return Mem_Flag_Type;
   --  Get current memory mode flags

   procedure Mem_Set_Mode (Mode : Mem_Flag_Type);
   --  Set current memory mode flags

   function Mem_Is_Address_Code_Memory (Address : Address_16_Bit)
                                        return Boolean;
   --  Returns whether the address contains RAM or something else.
   --   True:  code memory
   --   False: I/O memory or floating bus

   procedure Init_IO_Handlers;
   --  Initialize the default I/O handlers

   procedure Mem_Initialize;
   --  Initialize ROMs and clear RAM

   procedure Mem_Read_Floating_Bus
     (Read_Value : out Value_8_Bit; Executed_Cycles : Natural);
   --  Read floating bus address and advance cycle counter

   procedure Mem_Read_Floating_Bus
     (High_Bit : Boolean; Read_Value : out Value_8_Bit;
      Executed_Cycles : Natural);
   --  Read floating bus address, replacing high bit of result

   procedure Mem_Reset;

   procedure Mem_Reset_Paging;
   --  Called by:
   --    Soft-reset (Ctrl+Reset)
   --    Snapshot_Load_State()

   function Mem_Read_Random_Data
     (High_Bit : Boolean) return Value_8_Bit;
   --  Read random bus data and replace high bit with specified value
   --  Called by Disk ][ I/O only

   type Snapshot_Base_Memory is record
      Mem_Main       : Mem_Bank_64K;
      Mem_Aux        : Mem_Bank_64K;
      Mem_Mode       : Mem_Flag_Type;
      Last_Write_RAM : Boolean;
   end record;
   --  Snapshot data structure contains 128 KB RAM plus flags

   procedure Mem_Get_Snapshot (Snapshot : in out Snapshot_Base_Memory);
   --  Create snapshot of main and aux RAM

   procedure Mem_Set_Snapshot (Snapshot : Snapshot_Base_Memory);
   --  Restore snapshot of main and aux RAM

   procedure IO_Read_Null (Read_Value : out Value_8_Bit;
                           Cycles_Left : Natural);
   --  Default I/O read procedure (count cycles, read floating bus)

   procedure IO_Write_Null (Cycles_Left : Natural);
   --  Default I/O write procedure (count cycles)

   procedure Mem_Update_Paging
     (Initialize : Boolean; Update_Write_Only : Boolean);
   --  Update paging indices

   procedure Mem_Check_Paging (Address : Address_16_Bit;
                               Read_Value : out Value_8_Bit);
   --  Read byte containing paging mode and keyboard scan code

   function Mem_Get_Write_Page (Page : Value_8_Bit) return Mem_Page_Access;
   --  Returns access to the specified memory page to the CPU for writing

end Apple2.Memory;
