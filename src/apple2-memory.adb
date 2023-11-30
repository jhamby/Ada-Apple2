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

--  Description: Memory emulation
--
--  Author: Various

--  Adaptation for SDL and POSIX (l) by beom beotiger, Nov-Dec 2007

with Ada.Numerics.Discrete_Random;

with Apple2.CPU;          use Apple2.CPU;
with Apple2.Disk;         use Apple2.Disk;
with Apple2.Disk.ROM;     use Apple2.Disk.ROM;
with Apple2.Joystick;     use Apple2.Joystick;
with Apple2.Keyboard;     use Apple2.Keyboard;
with Apple2.Mockingboard; use Apple2.Mockingboard;
with Apple2.Printer;      use Apple2.Printer;
with Apple2.Printer.ROM;  use Apple2.Printer.ROM;
with Apple2.ROMs;         use Apple2.ROMs;
with Apple2.Serial;       use Apple2.Serial;
with Apple2.Serial.ROM;   use Apple2.Serial.ROM;
with Apple2.Speaker;      use Apple2.Speaker;
with Apple2.Video;        use Apple2.Video;

package body Apple2.Memory is

   Mem_Dirty : array (Value_8_Bit) of Value_8_Bit :=
     (others => 0);
   --  page dirty flags (accessed from Memory and Video)

   type Mem_Page_Access_Table is array (Value_8_Bit) of Mem_Page_Access;
   --  256-entry array of access to 256-byte memory pages

   Mem_Shadow : Mem_Page_Access_Table := (others => null);
   --  Shadow paging table

   Mem_Write : Mem_Page_Access_Table := (others => null);
   --  Memory write paging table

   Last_Write_RAM : Boolean := False;

   Mem_Main : aliased Mem_Bank_64K := (others => 0); --  64K main RAM

   Mem_ROM : aliased Mem_Range (0 .. 16#2FFF#) := (others => 0); --  12K ROM

   Mem_Image : aliased Mem_Bank_64K := (others => 0); --  visible 64K

   Cx_ROM_Internal : aliased Mem_Range (0 .. Cx_ROM_Size - 1) := (others => 0);

   Cx_ROM_Peripheral : aliased Mem_Range (0 .. Cx_ROM_Size - 1) :=
     (others => 0);

   RAM_Works_Pages : array (RAM_Works_Bank_Range) of aliased
     Mem_Bank_64K := (others => (others => 0));
   --  RAMWorks III array of 64K RAM pages (page 0 is default Mem_Aux)

   Mem_Aux : access Mem_Bank_64K := RAM_Works_Pages (0)'Access;
   --  Access to 64K aux RAM or RAMWorks III page

   Mem_Mode : Mem_Flag_Type := Flag_High_RAM_Bank_2 or Flag_Slot_CX_ROM or
     Flag_High_RAM_Write;
   --  Initial memory mode flags

   Mode_Changing : Boolean := False;

   RAM_Works_Active_Bank : RAM_Works_Bank_Range := 0;
   --  0 = aux 64K for: //e extended 80 Col card, or //c

   procedure IO_Read_Annunciator
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural);
   --  Read annunciator; does nothing on Apple IIe

   procedure IO_Write_Annunciator
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural);
   --  Write annunciator; do nothing

   procedure IO_Read_C00x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural);
   --  Keyboard

   procedure IO_Write_C00x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural);
   --  Memory / Video

   procedure IO_Read_C01x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural);
   --  Memory / Video

   procedure IO_Write_C01x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural);
   --  Keyboard

   procedure IO_Read_C02x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural);
   --  Cassette (not implemented)

   procedure IO_Write_C02x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural);
   --  Cassette (not implemented)

   procedure IO_Read_C03x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural);
   --  Speaker

   procedure IO_Write_C03x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural);
   --  Speaker

   procedure IO_Read_C04x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural);
   --  No mapping

   procedure IO_Write_C04x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural);
   --  No mapping

   procedure IO_Read_C05x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural);
   --  Video

   procedure IO_Write_C05x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural);
   --  Video / Memory

   procedure IO_Read_C06x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural);
   --  Joystick

   procedure IO_Write_C06x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural);
   --  No mapping

   procedure IO_Read_C07x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural);
   --  Joystick / Video

   procedure IO_Write_C07x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural);
   --  Joystick / RAMWorks

   procedure IO_Read_Cxxx
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural);
   --  Enabling expansion ROM ($C800..$CFFF]:
   --  . Enable if: Enable1 && Enable2
   --  . Enable1 = I/O SELECT' (6502 accesses $Csxx)
   --    - Reset when 6502 accesses $CFFF
   --  . Enable2 = I/O STROBE' (6502 accesses [$C800..$CFFF])

   procedure IO_Write_Cxxx
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural);
   --  No mapping

   IO_Select_Slot : Slot_Range := 0;
   --  I/O select for individual slots

   IO_Select_Internal_ROM : Boolean := False;

   type ROM_Type is (ROM_Null, ROM_Internal, ROM_Peripheral);

   Expansion_ROM_Type : ROM_Type := ROM_Null;

   Peripheral_ROM_Slot : Slot_Range := 0;

   function Mode_80_Store return Boolean;
   --  Memory mode is 80 store

   function Mode_Alt_ZP return Boolean;
   --  Memory mode is alt zero page

   function Mode_Aux_Read return Boolean;
   --  Memory mode is aux read

   function Mode_Aux_Write return Boolean;
   --  Memory mode is aux write

   function Mode_High_RAM_Bank_2 return Boolean;
   --  Memory mode is high RAM bank 2

   function Mode_High_RAM return Boolean;
   --  Memory mode is high RAM

   function Mode_Hi_Res return Boolean;
   --  Memory mode is hi-res

   function Mode_Page_2 return Boolean;
   --  Memory mode is page 2

   function Mode_Slot_C3_ROM return Boolean;
   --  Memory mode is slot C3 ROM

   function Mode_Slot_CX_ROM return Boolean;
   --  Memory mode is slot CX ROM

   function Mode_High_RAM_Write return Boolean;
   --  Memory mode is high RAM write

   procedure Reset_Paging (Initialize : Boolean);
   --  Reset paging tables

   procedure Mem_Set_Paging
     (PC, Address : Address_16_Bit; Is_Write : Boolean;
      Write_Value : Value_8_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural);
   --  Set or get memory address associated with paging

   function Next_Three_Code_Bytes (PC : Address_16_Bit) return Value_32_Bit;
   --  Helper to return next three bytes of code as little-endian value

   package Random_Byte is new Ada.Numerics.Discrete_Random (Value_8_Bit);

   Random_Generator : Random_Byte.Generator;
   --  Generate random bytes for Disk ][ emulation

   ------------------
   -- IO_Read_Null --
   ------------------

   procedure IO_Read_Null (Read_Value : out Value_8_Bit;
                           Cycles_Left : Natural) is
   begin
      Mem_Read_Floating_Bus (Read_Value, Cycles_Left);
   end IO_Read_Null;

   -------------------
   -- IO_Write_Null --
   -------------------

   procedure IO_Write_Null (Cycles_Left : Natural) is
   begin
      --  Note: original code only counted cycles for reads, not writes
      CPU_Calc_Cycles (Cycles_Left);
   end IO_Write_Null;

   -------------------------
   -- IO_Read_Annunciator --
   -------------------------

   procedure IO_Read_Annunciator
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC, Address);
   begin
      --  Apple //e ROM:
      --  . PC=FA6F: LDA $C058 (SETAN0)
      --  . PC=FA72: LDA $C05A (SETAN1)
      --  . PC=C2B5: LDA $C05D (CLRAN2)
      --  For //e & //c these locations are now used to enable/disable DHIRES
      Read_Value := 0;
      CPU_Calc_Cycles (Cycles_Left);
   end IO_Read_Annunciator;

   --------------------------
   -- IO_Write_Annunciator --
   --------------------------

   procedure IO_Write_Annunciator
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC, Address, Write_Value);
   begin
      CPU_Calc_Cycles (Cycles_Left);
   end IO_Write_Annunciator;

   ------------------
   -- IO_Read_C00x --
   ------------------

   procedure IO_Read_C00x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC, Address, Cycles_Left);
   begin
      Keyb_Read_Data (Read_Value);
   end IO_Read_C00x;

   -------------------
   -- IO_Write_C00x --
   -------------------

   procedure IO_Write_C00x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
      Ignore : Value_8_Bit;  --  ignore read value
   begin
      if (Address and 16#0F#) <= 16#0B# then
         Mem_Set_Paging (PC, Address, True, Write_Value, Ignore,
                         Cycles_Left);
      else
         Video_Set_Mode (Address);
      end if;
   end IO_Write_C00x;

   ------------------
   -- IO_Read_C01x --
   ------------------

   procedure IO_Read_C01x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC);
      Offset : constant Value_4_Bit := Value_4_Bit (Address and 16#0F#);
   begin
      case Offset is
      when 16#00# =>
         Keyb_Read_Flag (Read_Value);

      when 16#01# .. 16#08# | 16#0C# .. 16#0D# =>
         Mem_Check_Paging (Address, Read_Value);

      when 16#09# =>
         Video_Check_VBL (Read_Value, Cycles_Left);

      when 16#0A# .. 16#0B# | 16#0E# .. 16#0F# =>
         Video_Check_Mode (Address, Read_Value, Cycles_Left);

      end case;
   end IO_Read_C01x;

   -------------------
   -- IO_Write_C01x --
   -------------------

   procedure IO_Write_C01x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC, Address, Write_Value, Cycles_Left);
      Ignore : Value_8_Bit;  --  ignore read value
   begin
      Keyb_Read_Flag (Ignore);
   end IO_Write_C01x;

   ------------------
   -- IO_Read_C02x --
   ------------------

   procedure IO_Read_C02x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC, Address);
   begin
      Mem_Read_Floating_Bus (Read_Value, Cycles_Left);
   end IO_Read_C02x;

   -------------------
   -- IO_Write_C02x --
   -------------------

   procedure IO_Write_C02x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
   begin
      null;
   end IO_Write_C02x;

   ------------------
   -- IO_Read_C03x --
   ------------------

   procedure IO_Read_C03x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC, Address);
   begin
      Read_Value := 0;
      Spkr_Toggle (Cycles_Left);
   end IO_Read_C03x;

   -------------------
   -- IO_Write_C03x --
   -------------------

   procedure IO_Write_C03x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC, Address, Write_Value);
   begin
      Spkr_Toggle (Cycles_Left);
   end IO_Write_C03x;

   ------------------
   -- IO_Read_C04x --
   ------------------

   procedure IO_Read_C04x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC, Address);
   begin
      Mem_Read_Floating_Bus (Read_Value, Cycles_Left);
   end IO_Read_C04x;

   -------------------
   -- IO_Write_C04x --
   -------------------

   procedure IO_Write_C04x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
   begin
      null;
   end IO_Write_C04x;

   ------------------
   -- IO_Read_C05x --
   ------------------

   procedure IO_Read_C05x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      Offset : constant Value_4_Bit := Value_4_Bit (Address and 16#0F#);
   begin
      case Offset is
      when 16#00# .. 16#03# | 16#0E# .. 16#0F# =>
         Video_Set_Mode (Address);
         Mem_Read_Floating_Bus (Read_Value, Cycles_Left);

      when 16#04# .. 16#07# =>
         Mem_Set_Paging (PC, Address, False, 0, Read_Value, Cycles_Left);

      when 16#08# .. 16#0D# =>
         IO_Read_Annunciator (PC, Address, Read_Value, Cycles_Left);

      end case;
   end IO_Read_C05x;

   -------------------
   -- IO_Write_C05x --
   -------------------

   procedure IO_Write_C05x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
      Ignore : Value_8_Bit;  --  ignore read value
      Offset : constant Value_4_Bit := Value_4_Bit (Address and 16#0F#);
   begin
      case Offset is
      when 16#00# .. 16#03# | 16#0E# .. 16#0F# =>
         Video_Set_Mode (Address);

      when 16#04# .. 16#07# =>
         Mem_Set_Paging (PC, Address, True, Write_Value, Ignore, Cycles_Left);

      when 16#08# .. 16#0D# =>
         IO_Write_Annunciator (PC, Address, Write_Value, Cycles_Left);

      end case;
   end IO_Write_C05x;

   ------------------
   -- IO_Read_C06x --
   ------------------

   procedure IO_Read_C06x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC);
      Offset : constant Value_4_Bit := Value_4_Bit (Address and 16#0F#);
   begin
      case Offset is
      when 16#00# | 16#08# .. 16#0F# =>
         IO_Read_Null (Read_Value, Cycles_Left);

      when 16#01# .. 16#03# =>
         Joy_Read_Button (Address, Read_Value, Cycles_Left);

      when 16#04# .. 16#07# =>
         Joy_Read_Position (Address, Read_Value, Cycles_Left);

      end case;
   end IO_Read_C06x;

   -------------------
   -- IO_Write_C06x --
   -------------------

   procedure IO_Write_C06x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
   begin
      null;
   end IO_Write_C06x;

   ------------------
   -- IO_Read_C07x --
   ------------------

   procedure IO_Read_C07x
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC);
      Offset : constant Value_4_Bit := Value_4_Bit (Address and 16#0F#);
   begin
      case Offset is
      when 16#00# =>
         Joy_Reset_Position (Cycles_Left);
         Mem_Read_Floating_Bus (Read_Value, Cycles_Left);

      when 16#01# .. 16#0E# =>
         IO_Read_Null (Read_Value, Cycles_Left);

      when 16#0F# =>
         Video_Check_Mode (Address, Read_Value, Cycles_Left);

      end case;
   end IO_Read_C07x;

   -------------------
   -- IO_Write_C07x --
   -------------------

   procedure IO_Write_C07x
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
      Ignore : Value_8_Bit;  --  ignore read value
      Offset : constant Value_4_Bit := Value_4_Bit (Address and 16#0F#);
   begin
      case Offset is
      when 16#00# =>
         Joy_Reset_Position (Cycles_Left);

      when 16#01# | 16#03# =>
         Mem_Set_Paging (PC, Address, True, Write_Value, Ignore, Cycles_Left);

      when 16#02# | 16#04# .. 16#0F# =>
         IO_Write_Null (Cycles_Left);

      end case;
   end IO_Write_C07x;

   -----------------
   -- Mem_IO_Read --
   -----------------

   procedure Mem_IO_Read
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      Index : constant Value_8_Bit := Value_8_Bit (Shift_Right (Address, 4)
                                                   and 16#FF#);
   begin
      --  The configuration is now hardcoded to enable SPARK static analysis.
      --  Hopefully it's not much slower than the original function pointers.
      case Index is
      when 16#00# =>
         IO_Read_C00x (PC, Address, Read_Value, Cycles_Left);
      when 16#01# =>
         IO_Read_C01x (PC, Address, Read_Value, Cycles_Left);
      when 16#02# =>
         IO_Read_C02x (PC, Address, Read_Value, Cycles_Left);
      when 16#03# =>
         IO_Read_C03x (PC, Address, Read_Value, Cycles_Left);
      when 16#04# =>
         IO_Read_C04x (PC, Address, Read_Value, Cycles_Left);
      when 16#05# =>
         IO_Read_C05x (PC, Address, Read_Value, Cycles_Left);
      when 16#06# =>
         IO_Read_C06x (PC, Address, Read_Value, Cycles_Left);
      when 16#07# =>
         IO_Read_C07x (PC, Address, Read_Value, Cycles_Left);
      when 16#08# =>
         --  slot 0
         Mem_Set_Paging (PC, Address, False, 0, Read_Value, Cycles_Left);
      when 16#09# =>
         --  slot 1 (parallel printer card)
         Print_Status (Read_Value);
      when 16#0A# =>
         --  slot 2 (super serial card)
         SSC_Read (Address, Read_Value, Cycles_Left);
      when 16#0B# =>
         --  slot 3 (no card)
         IO_Read_Null (Read_Value, Cycles_Left);
      when 16#0C# =>
         --  slot 4 (Mockingboard or mouse)
         Phasor_IO (Address);
         Mem_Read_Floating_Bus (Read_Value, Cycles_Left);
      when 16#0D# =>
         --  slot 5 (Phasor sound card)
         Phasor_IO (Address);
         Mem_Read_Floating_Bus (Read_Value, Cycles_Left);
      when 16#0E# =>
         --  slot 6 (Disk ][)
         Disk_IO_Read (Address, Read_Value, Cycles_Left);
      when 16#0F# =>
         --  slot 7 (no card)
         IO_Read_Null (Read_Value, Cycles_Left);
      when 16#40# .. 16#5F# =>
         --  slots 4 and 5 $Cxxx space (Mockingboard / Phasor)
         MB_Read (Address, Read_Value, Cycles_Left);
      when others =>
         IO_Read_Cxxx (PC, Address, Read_Value, Cycles_Left);
      end case;
   end Mem_IO_Read;

   ------------------
   -- Mem_IO_Write --
   ------------------

   procedure Mem_IO_Write
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
      Index : constant Value_8_Bit := Value_8_Bit (Shift_Right (Address, 4)
                                                   and 16#FF#);
      Ignore : Value_8_Bit;  --  ignore read value
   begin
      --  The configuration is now hardcoded to enable SPARK static analysis.
      --  Hopefully it's not much slower than the original function pointers.
      case Index is
      when 16#00# =>
         IO_Write_C00x (PC, Address, Write_Value, Cycles_Left);
      when 16#01# =>
         IO_Write_C01x (PC, Address, Write_Value, Cycles_Left);
      when 16#02# =>
         IO_Write_C02x (PC, Address, Write_Value, Cycles_Left);
      when 16#03# =>
         IO_Write_C03x (PC, Address, Write_Value, Cycles_Left);
      when 16#04# =>
         IO_Write_C04x (PC, Address, Write_Value, Cycles_Left);
      when 16#05# =>
         IO_Write_C05x (PC, Address, Write_Value, Cycles_Left);
      when 16#06# =>
         IO_Write_C06x (PC, Address, Write_Value, Cycles_Left);
      when 16#07# =>
         IO_Write_C07x (PC, Address, Write_Value, Cycles_Left);
      when 16#08# =>
         --  slot 0
         Mem_Set_Paging (PC, Address, True, Write_Value, Ignore, Cycles_Left);
      when 16#09# =>
         --  slot 1 (parallel printer card)
         Print_Transmit (Write_Value);
      when 16#0A# =>
         --  slot 2 (super serial card)
         SSC_Write (Address, Write_Value, Cycles_Left);
      when 16#0B# =>
         --  slot 3 (no card)
         IO_Write_Null (Cycles_Left);
      when 16#0C# =>
         --  slot 4 (Mockingboard or mouse)
         Phasor_IO (Address);
         CPU_Calc_Cycles (Cycles_Left);
      when 16#0D# =>
         --  slot 5 (Phasor sound card)
         Phasor_IO (Address);
         CPU_Calc_Cycles (Cycles_Left);
      when 16#0E# =>
         --  slot 6 (Disk ][)
         Disk_IO_Write (Address, Write_Value, Cycles_Left);
      when 16#0F# =>
         --  slot 7 (no card)
         IO_Write_Null (Cycles_Left);
      when 16#40# .. 16#5F# =>
         --  slots 4 and 5 $Cxxx space (Mockingboard / Phasor)
         MB_Write (Address, Write_Value, Cycles_Left);
      when others =>
         IO_Write_Cxxx (PC, Address, Write_Value, Cycles_Left);
      end case;
   end Mem_IO_Write;

   -------------------
   -- Mode_80_Store --
   -------------------

   function Mode_80_Store return Boolean is
   begin
      return (Mem_Mode and Flag_80_Store) /= 0;
   end Mode_80_Store;

   -----------------
   -- Mode_Alt_ZP --
   -----------------

   function Mode_Alt_ZP return Boolean is
   begin
      return (Mem_Mode and Flag_Alt_ZP) /= 0;
   end Mode_Alt_ZP;

   -------------------
   -- Mode_Aux_Read --
   -------------------

   function Mode_Aux_Read return Boolean is
   begin
      return (Mem_Mode and Flag_Aux_Read) /= 0;
   end Mode_Aux_Read;

   --------------------
   -- Mode_Aux_Write --
   --------------------

   function Mode_Aux_Write return Boolean is
   begin
      return (Mem_Mode and Flag_Aux_Write) /= 0;
   end Mode_Aux_Write;

   --------------------------
   -- Mode_High_RAM_Bank_2 --
   --------------------------

   function Mode_High_RAM_Bank_2 return Boolean is
   begin
      return (Mem_Mode and Flag_High_RAM_Bank_2) /= 0;
   end Mode_High_RAM_Bank_2;

   -------------------
   -- Mode_High_RAM --
   -------------------

   function Mode_High_RAM return Boolean is
   begin
      return (Mem_Mode and Flag_High_RAM) /= 0;
   end Mode_High_RAM;

   -----------------
   -- Mode_Hi_Res --
   -----------------

   function Mode_Hi_Res return Boolean is
   begin
      return (Mem_Mode and Flag_Hi_Res) /= 0;
   end Mode_Hi_Res;

   -----------------
   -- Mode_Page_2 --
   -----------------

   function Mode_Page_2 return Boolean is
   begin
      return (Mem_Mode and Flag_Page_2) /= 0;
   end Mode_Page_2;

   ----------------------
   -- Mode_Slot_C3_ROM --
   ----------------------

   function Mode_Slot_C3_ROM return Boolean is
   begin
      return (Mem_Mode and Flag_Slot_C3_ROM) /= 0;
   end Mode_Slot_C3_ROM;

   ----------------------
   -- Mode_Slot_CX_ROM --
   ----------------------

   function Mode_Slot_CX_ROM return Boolean is
   begin
      return (Mem_Mode and Flag_Slot_CX_ROM) /= 0;
   end Mode_Slot_CX_ROM;

   -------------------------
   -- Mode_High_RAM_Write --
   -------------------------

   function Mode_High_RAM_Write return Boolean is
   begin
      return (Mem_Mode and Flag_High_RAM_Write) /= 0;
   end Mode_High_RAM_Write;

   ------------------
   -- IO_Read_Cxxx --
   ------------------

   procedure IO_Read_Cxxx
     (PC, Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (PC);
      IO_Strobe : Boolean := False;
      Slot : Slot_Range;
   begin
      if Address = 16#CFFF# then
         --  Disable expansion ROM at [$C800..$CFFF]
         --  SSC will disable on an access to $CFxx, but ROM only writes to
         --  $CFFF, so it doesn't matter
         IO_Select_Slot := 0;
         IO_Select_Internal_ROM := False;
         Peripheral_ROM_Slot := 0;

         if Mode_Slot_CX_ROM then
            --  unset Slot_CX_ROM ensures that internal rom stays switched in
            Cx_ROM_Peripheral (16#0800# .. 16#0FFF#) := (others => 0);
            Mem_Image (16#C800# .. 16#CFFF#) := (others => 0);
            Expansion_ROM_Type := ROM_Null;
         end if;
         --  NB. IO_SELECT won't get set, so ROM won't be switched back in...
      end if;

      if Is_Apple2 or Mode_Slot_CX_ROM then
         if Address >= 16#C100# and Address <= 16#C7FF# then
            Slot := Slot_Range (Shift_Right (Address, 8) and 7);
            if Slot = 2 then
               --  Note: add any other slots with expansion ROMs here
               IO_Select_Slot := Slot;
            elsif not Mode_Slot_C3_ROM then
               IO_Select_Internal_ROM := True;
               --  Slot 3 & Internal ROM
            end if;
         elsif Address >= 16#C800# and Address <= 16#CFFF# then
            IO_Strobe := True;
         end if;

         if IO_Select_Slot /= 0 and IO_Strobe then
            --  Enable Peripheral Expansion ROM
            Slot := IO_Select_Slot;
            if Slot = 2 and Peripheral_ROM_Slot /= Slot then
               --  Note: add any other slots with expansion ROMs here
               Cx_ROM_Peripheral (16#0800# .. 16#0FFF#) := SSC_ROM;
               --  SSC is the only emulated card with an expansion ROM
               Mem_Image (16#C800# .. 16#CFFF#) := SSC_ROM;
               Expansion_ROM_Type := ROM_Peripheral;
               Peripheral_ROM_Slot := Slot;
            end if;
         elsif IO_Select_Internal_ROM and IO_Strobe and
           Expansion_ROM_Type /= ROM_Internal
         then
            --  Enable Internal ROM (get this for PR#3)
            Mem_Image (16#C800# .. 16#CFFF#) :=
              Cx_ROM_Internal (16#0800# .. 16#0FFF#);
            Expansion_ROM_Type := ROM_Internal;
            Peripheral_ROM_Slot := 0;
         end if;
      end if;

      if not Is_Apple2 and not Mode_Slot_CX_ROM then
         --  not Mode_Slot_C3_ROM = Internal ROM: $C300-C3FF
         --  not Mode_Slot_CX_ROM = Internal ROM: $C100-CFFF

         if Address >= 16#C100# and Address <= 16#C7FF# then
            --  Don't care about state of Slot_C3_ROM
            IO_Select_Internal_ROM := True;
         elsif Address >= 16#C800# and Address <= 16#CFFF# then
            IO_Strobe := True;
         end if;

         if not Mode_Slot_CX_ROM and IO_Select_Internal_ROM and
           IO_Strobe and Expansion_ROM_Type /= ROM_Internal
         then
            --  Enable Internal ROM
            Mem_Image (16#C800# .. 16#CFFF#) :=
              Cx_ROM_Internal (16#0800# .. 16#0FFF#);
            Expansion_ROM_Type := ROM_Internal;
            Peripheral_ROM_Slot := 0;
         end if;
      end if;

      if Expansion_ROM_Type = ROM_Null and Address >= 16#C800# then
         IO_Read_Null (Read_Value, Cycles_Left);
      else
         Read_Value := Mem_Image (Address);
      end if;
   end IO_Read_Cxxx;

   -------------------
   -- IO_Write_Cxxx --
   -------------------

   procedure IO_Write_Cxxx
     (PC, Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
   begin
      null;
   end IO_Write_Cxxx;

   ----------------------
   -- Init_IO_Handlers --
   ----------------------

   procedure Init_IO_Handlers is
   begin
      IO_Select_Slot := 0;
      IO_Select_Internal_ROM := False;
      Expansion_ROM_Type := ROM_Null;
      Peripheral_ROM_Slot := 0;
   end Init_IO_Handlers;

   ------------------
   -- Mem_Get_Mode --
   ------------------

   function Mem_Get_Mode return Mem_Flag_Type is
   begin
      return Mem_Mode;
   end Mem_Get_Mode;

   ------------------
   -- Mem_Set_Mode --
   ------------------

   procedure Mem_Set_Mode (Mode : Mem_Flag_Type) is
   begin
      Mem_Mode := Mode;
   end Mem_Set_Mode;

   -----------------------
   -- + (Mem_Flag_Type) --
   -----------------------

   function "+" (L, R : Mem_Flag_Type) return Mem_Flag_Type is
   begin
      return L or R;
   end "+";

   ------------------
   -- Reset_Paging --
   ------------------

   procedure Reset_Paging (Initialize : Boolean) is
   begin
      Last_Write_RAM := False;
      Mem_Mode := Flag_High_RAM_Bank_2 or Flag_Slot_CX_ROM or
        Flag_High_RAM_Write;
      Mem_Update_Paging (Initialize, False);
   end Reset_Paging;

   ------------------------
   -- Mem_Get_Write_Page --
   ------------------------

   function Mem_Get_Write_Page (Page : Value_8_Bit) return Mem_Page_Access is
   begin
      return Mem_Write (Page);
   end Mem_Get_Write_Page;

   -----------------------
   -- Mem_Update_Paging --
   -----------------------

   procedure Mem_Update_Paging (Initialize, Update_Write_Only : Boolean) is
      Old_Shadow : Mem_Page_Access_Table;
      Offset : Address_16_Bit;
      Page : Mem_Page_Access;
   begin
      --  Save the current paging shadow table
      if not (Initialize or Update_Write_Only) then
         Old_Shadow := Mem_Shadow;
      end if;

      --  Update the paging tables based on the new paging switch values
      if Initialize then
         for I in Value_8_Bit (16#00#) .. Value_8_Bit (16#BF#) loop
            Offset := Shift_Left (Address_16_Bit (I), 8);
            Page := Mem_Image (Offset .. Offset + 255)'Unrestricted_Access;
            Mem_Write (I) := Page;
         end loop;

         for I in Value_8_Bit (16#C0#) .. Value_8_Bit (16#CF#) loop
            Mem_Write (I) := null;
         end loop;
      end if;

      if not Update_Write_Only then
         for I in Value_8_Bit (16#00#) .. Value_8_Bit (16#01#) loop
            Offset := Shift_Left (Address_16_Bit (I), 8);
            if Mode_Alt_ZP then
               Page := Mem_Aux.all (Offset .. Offset + 255)
                 'Unrestricted_Access;
            else
               Page := Mem_Main (Offset .. Offset + 255)
                 'Unrestricted_Access;
            end if;
            Mem_Shadow (I) := Page;
         end loop;
      end if;

      for I in Value_8_Bit (16#02#) .. Value_8_Bit (16#BF#) loop
         Offset := Shift_Left (Address_16_Bit (I), 8);
         if Mode_Aux_Read then
            Page := Mem_Aux.all (Offset .. Offset + 255)
              'Unrestricted_Access;
         else
            Page := Mem_Main (Offset .. Offset + 255)'Unrestricted_Access;
         end if;
         Mem_Shadow (I) := Page;

         if Mode_Aux_Read = Mode_Aux_Write then
            Page := Mem_Image (Offset .. Offset + 255)'Unrestricted_Access;
         elsif Mode_Aux_Write then
            Page := Mem_Aux.all (Offset .. Offset + 255)'Unrestricted_Access;
         else
            Page := Mem_Main (Offset .. Offset + 255)'Unrestricted_Access;
         end if;
         Mem_Write (I) := Page;
      end loop;

      if not Update_Write_Only then
         for I in Value_8_Bit (16#C0#) .. Value_8_Bit (16#C7#) loop
            Offset := Shift_Left (Address_16_Bit (I and 16#0F#), 8);
            if I = 16#C3# then
               if Mode_Slot_C3_ROM and Mode_Slot_CX_ROM then
                  Mem_Shadow (I) := Cx_ROM_Peripheral (Offset .. Offset + 255)
                    'Unrestricted_Access;
                  --  C300..C3FF - Slot 3 ROM (all 0x00's)
               else
                  Mem_Shadow (I) := Cx_ROM_Internal (Offset .. Offset + 255)
                    'Unrestricted_Access;
                  --  C300..C3FF - Internal ROM
               end if;
            else
               if Mode_Slot_CX_ROM then
                  Mem_Shadow (I) := Cx_ROM_Peripheral (Offset .. Offset + 255)
                    'Unrestricted_Access;
                  --  C000..C7FF - SSC/Disk ][/etc
               else
                  Mem_Shadow (I) := Cx_ROM_Internal (Offset .. Offset + 255)
                    'Unrestricted_Access;
                  --  C000..C7FF - Internal ROM
               end if;
            end if;
         end loop;

         for I in Value_8_Bit (16#C8#) .. Value_8_Bit (16#CF#) loop
            Offset := Shift_Left (Address_16_Bit (I and 16#0F#), 8);
            Mem_Shadow (I) := Cx_ROM_Internal (Offset .. Offset + 255)
              'Unrestricted_Access;
            --  C800..CFFF - Internal ROM
         end loop;
      end if;

      for I in Value_8_Bit (16#D0#) .. Value_8_Bit (16#DF#) loop
         if Mode_High_RAM_Bank_2 then
            Offset := 0;  --  bank offset
         else
            Offset := 16#1000#;  --  bank offset
         end if;

         if Mode_High_RAM then
            Offset := Shift_Left (Address_16_Bit (I), 8) - Offset;
            if Mode_Alt_ZP then
               Mem_Shadow (I) := Mem_Aux.all (Offset .. Offset + 255)
                 'Unrestricted_Access;
            else
               Mem_Shadow (I) := Mem_Main (Offset .. Offset + 255)
                 'Unrestricted_Access;
            end if;
         else
            Offset := Shift_Left (Address_16_Bit (I - 16#D0#), 8);
            Mem_Shadow (I) := Mem_ROM (Offset .. Offset + 255)
              'Unrestricted_Access;
         end if;

         if Mode_High_RAM_Write then
            if Mode_High_RAM then
               Offset := Shift_Left (Address_16_Bit (I), 8);
               Mem_Write (I) := Mem_Image (Offset .. Offset + 255)
                 'Unrestricted_Access;
            elsif Mode_Alt_ZP then
               Offset := Shift_Left (Address_16_Bit (I), 8) - Offset;
               Mem_Write (I) := Mem_Aux.all (Offset .. Offset + 255)
                 'Unrestricted_Access;
            else
               Offset := Shift_Left (Address_16_Bit (I), 8) - Offset;
               Mem_Write (I) := Mem_Main (Offset .. Offset + 255)
                 'Unrestricted_Access;
            end if;
         else
            Mem_Write (I) := null;
         end if;
      end loop;

      for I in Value_8_Bit (16#E0#) .. Value_8_Bit (16#FF#) loop
         if Mode_High_RAM then
            Offset := Shift_Left (Address_16_Bit (I), 8);
            if Mode_Alt_ZP then
               Mem_Shadow (I) := Mem_Aux.all (Offset .. Offset + 255)
                 'Unrestricted_Access;
            else
               Mem_Shadow (I) := Mem_Main (Offset .. Offset + 255)
                 'Unrestricted_Access;
            end if;
         else
            Offset := Shift_Left (Address_16_Bit (I - 16#D0#), 8);
            Mem_Shadow (I) := Mem_ROM (Offset .. Offset + 255)
              'Unrestricted_Access;
         end if;

         Offset := Shift_Left (Address_16_Bit (I), 8);
         if Mode_High_RAM_Write then
            if Mode_High_RAM then
               Mem_Write (I) := Mem_Image (Offset .. Offset + 255)
                 'Unrestricted_Access;
            elsif Mode_Alt_ZP then
               Mem_Write (I) := Mem_Aux.all (Offset .. Offset + 255)
                 'Unrestricted_Access;
            else
               Mem_Write (I) := Mem_Main (Offset .. Offset + 255)
                 'Unrestricted_Access;
            end if;
         else
            Mem_Write (I) := null;
         end if;
      end loop;

      if Mode_80_Store then
         for I in Value_8_Bit (16#04#) .. Value_8_Bit (16#07#) loop
            Offset := Shift_Left (Address_16_Bit (I), 8);
            if Mode_Page_2 then
               Mem_Shadow (I) := Mem_Aux.all (Offset .. Offset + 255)
                 'Unrestricted_Access;
            else
               Mem_Shadow (I) := Mem_Main (Offset .. Offset + 255)
                 'Unrestricted_Access;
            end if;

            Mem_Write (I) := Mem_Image (Offset .. Offset + 255)
              'Unrestricted_Access;
         end loop;

         if Mode_Hi_Res then
            for I in Value_8_Bit (16#20#) .. Value_8_Bit (16#3F#) loop
               Offset := Shift_Left (Address_16_Bit (I), 8);
               if Mode_Page_2 then
                  Mem_Shadow (I) := Mem_Aux.all (Offset .. Offset + 255)
                    'Unrestricted_Access;
               else
                  Mem_Shadow (I) := Mem_Main (Offset .. Offset + 255)
                    'Unrestricted_Access;
               end if;

               Mem_Write (I) := Mem_Image (Offset .. Offset + 255)
                 'Unrestricted_Access;
            end loop;
         end if;
      end if;

      --  Move memory back and forth as necessary between the shadow areas and
      --  the main ram image to keep both sets of memory consistent with the
      --  new paging shadow table
      if not Update_Write_Only then
         for I in Value_8_Bit (16#00#) .. Value_8_Bit (16#FF#) loop
            if Initialize or Old_Shadow (I) /= Mem_Shadow (I) then
               Offset := Shift_Left (Address_16_Bit (I), 8);
               if not Initialize and ((Mem_Dirty (I) and 1) /= 0 or I <= 1)
               then
                  Mem_Dirty (I) := Mem_Dirty (I) and 16#FE#;
                  Old_Shadow (I).all := Mem_Image (Offset .. Offset + 255);
               end if;
               Mem_Image (Offset .. Offset + 255) := Mem_Shadow (I).all;
            end if;
         end loop;
      end if;
   end Mem_Update_Paging;

   ----------------------
   -- Mem_Check_Paging --
   ----------------------

   procedure Mem_Check_Paging (Address : Address_16_Bit;
                               Read_Value : out Value_8_Bit) is
      Mode_Value : Boolean;
      Key_Value : Value_8_Bit;
   begin
      --  TODO: >= Apple 2e only?
      case Value_8_Bit (Address and 16#FF#) is
      when 16#11# =>
         Mode_Value := Mode_High_RAM_Bank_2;
      when 16#12# =>
         Mode_Value := Mode_High_RAM;
      when 16#13# =>
         Mode_Value := Mode_Aux_Read;
      when 16#14# =>
         Mode_Value := Mode_Aux_Write;
      when 16#15# =>
         Mode_Value := not Mode_Slot_CX_ROM;
      when 16#16# =>
         Mode_Value := Mode_Alt_ZP;
      when 16#17# =>
         Mode_Value := Mode_Slot_C3_ROM;
      when 16#18# =>
         Mode_Value := Mode_80_Store;
      when 16#1C# =>
         Mode_Value := Mode_Page_2;
      when 16#1D# =>
         Mode_Value := Mode_Hi_Res;
      when others =>
         Mode_Value := False;
      end case;

      Key_Value := Keyb_Get_Keycode;
      if Mode_Value then
         Read_Value := Key_Value or 16#80#;
      else
         Read_Value := Key_Value;
      end if;
   end Mem_Check_Paging;

   -----------------------
   -- Mem_Get_Aux_Range --
   -----------------------

   procedure Mem_Get_Aux_Range
     (Offset, Length : Address_16_Bit; Mem_Range : out Mem_Range_Access) is
      Offset_Page : constant Value_8_Bit :=
        Value_8_Bit (Shift_Right (Offset, 8));
      Offset_Page_Address : constant Address_16_Bit := Offset and 16#FF00#;
      Aux_Bank : access Mem_Bank_64K := Mem_Aux;
   begin
      --  Special handling for RAMWorks to use bank 0 sometimes
      if ((Mode_Page_2 and Mode_80_Store) or Video_Mode_80_Column) and
        ((Offset_Page >= 16#04# and Offset_Page < 16#08#) or
             (Mode_Hi_Res and
                  Offset_Page >= 16#20# and Offset_Page < 16#40#))
      then
         Aux_Bank := RAM_Works_Pages (0)'Access;
      end if;

      if Mem_Shadow (Offset_Page) = Aux_Bank (Offset_Page_Address ..
                                                Offset_Page_Address + 255)
          'Unrestricted_Access
      then
         Mem_Range := Mem_Image (Offset .. Offset + Length - 1)
           'Unrestricted_Access;
      else
         Mem_Range := Aux_Bank (Offset .. Offset + Length - 1)
           'Unrestricted_Access;
      end if;
   end Mem_Get_Aux_Range;

   ------------------------
   -- Mem_Get_Main_Range --
   ------------------------

   procedure Mem_Get_Main_Range
     (Offset, Length : Address_16_Bit; Mem_Range : out Mem_Range_Access) is
      Offset_Page : constant Value_8_Bit :=
        Value_8_Bit (Shift_Right (Offset, 8));
      Offset_Page_Address : constant Address_16_Bit := Offset and 16#FF00#;
   begin
      if Mem_Shadow (Offset_Page) = Mem_Main (Offset_Page_Address ..
                                                Offset_Page_Address + 255)
          'Unrestricted_Access
      then
         Mem_Range := Mem_Image (Offset .. Offset + Length - 1)
           'Unrestricted_Access;
      else
         Mem_Range := Mem_Main (Offset .. Offset + Length - 1)
           'Unrestricted_Access;
      end if;
   end Mem_Get_Main_Range;

   -------------------------------
   -- Mem_Get_Cx_ROM_Peripheral --
   -------------------------------

   procedure Mem_Get_Cx_ROM_Peripheral (Mem_Range : out Mem_Range_Access) is
   begin
      Mem_Range := Cx_ROM_Peripheral'Unrestricted_Access;
   end Mem_Get_Cx_ROM_Peripheral;

   --------------------------------
   -- Mem_Is_Address_Code_Memory --
   --------------------------------

   function Mem_Is_Address_Code_Memory (Address : Address_16_Bit)
                                        return Boolean is
   begin
      if Address < 16#C000# or Address > Firmware_Expansion_End then
         return True;   --  Assume all A][ types have at least 48K
      end if;

      if Address < Apple_Slot_Begin then
         return False;  --  [$C000 .. $C0FF]
      end if;

      if not Is_Apple2 and Mode_Slot_CX_ROM then
         return True;   --  [$C100..C7FF] //e or Enhanced //e internal ROM
      end if;

      if not Is_Apple2 and not Mode_Slot_C3_ROM and
        Shift_Right (Address, 8) = 16#C3#
      then
         return True;   --  [$C300..C3FF] //e or Enhanced //e internal ROM
      end if;

      if Address <= Apple_Slot_End then
         declare
            Slot : constant Address_16_Bit := Shift_Right (Address, 8) and 7;
         begin
            --  [$C100 .. $C7FF] assume cards in all slots except 3 and 7
            --  TODO: hard drive will be added at slot 7
            return Slot /= 3 and Slot /= 7;
         end;
      end if;

      --  [$C800 .. $CFFF]
      return Expansion_ROM_Type /= ROM_Null;
   end Mem_Is_Address_Code_Memory;

   --------------------
   -- Mem_Initialize --
   --------------------

   procedure Mem_Initialize is
      ROM_Data : access constant Mem_Range;
      ROM_Size : Address_16_Bit;
   begin
      Random_Byte.Reset (Random_Generator);

      case Get_Machine_Type is
      when Apple_2 =>
         ROM_Data := Apple_2_ROM'Unrestricted_Access;
         ROM_Size := Apple_2_ROM_Size;
      when Apple_2_Plus =>
         ROM_Data := Apple_2_Plus_ROM'Unrestricted_Access;
         ROM_Size := Apple_2_ROM_Size;
      when Apple_2e =>
         ROM_Data := Apple_2e_ROM'Unrestricted_Access;
         ROM_Size := Apple_2e_ROM_Size;
      when others =>
         ROM_Data := Apple_2e_Enhanced_ROM'Unrestricted_Access;
         ROM_Size := Apple_2e_ROM_Size;
      end case;

      Cx_ROM_Peripheral := (others => 0);

      if ROM_Size = Apple_2e_ROM_Size then
         Cx_ROM_Internal := ROM_Data.all (0 .. Cx_ROM_Size - 1);
         Mem_ROM := ROM_Data.all (Cx_ROM_Size .. Apple_2e_ROM_Size - 1);
      else
         Cx_ROM_Internal := (others => 0);
         Mem_ROM := ROM_Data.all (0 .. Apple_2_ROM_Size - 1);
      end if;

      Cx_ROM_Peripheral (16#0100# .. 16#01FF#) := Printer_ROM;
      --  $C100 : Parallel printer f/w
      Cx_ROM_Peripheral (16#0200# .. 16#02FF#) :=
        SSC_ROM (SSC_Slot_Firmware_Offset .. SSC_Slot_Firmware_Offset + 255);
      --  $C200 : SSC slot f/w (256 byte section of 2KB ROM)
      Cx_ROM_Peripheral (16#0600# .. 16#06FF#) := Disk_ROM;
      --  HACK! REMOVE A WAIT ROUTINE FROM THE DISK CONTROLLER'S FIRMWARE
      Cx_ROM_Peripheral (16#064C#) := 16#A9#;
      Cx_ROM_Peripheral (16#064D#) := 16#00#;
      Cx_ROM_Peripheral (16#064E#) := 16#EA#;
      --  $C600 : Disk ][ f/w
      --  TODO: $C700 : HDD f/w
      Mem_Reset;
   end Mem_Initialize;

   ---------------------------
   -- Mem_Read_Floating_Bus --
   ---------------------------

   procedure Mem_Read_Floating_Bus
     (Read_Value : out Value_8_Bit; Executed_Cycles : Natural) is
      VBL_Bar_Ignore : Boolean;
      Address : Address_16_Bit;
   begin
      Video_Get_Scanner_Address (Executed_Cycles, VBL_Bar_Ignore, Address);
      Read_Value := Mem_Image (Address);
   end Mem_Read_Floating_Bus;

   --------------------------------------
   -- Mem_Read_Floating_Bus (high bit) --
   --------------------------------------

   procedure Mem_Read_Floating_Bus
     (High_Bit : Boolean; Read_Value : out Value_8_Bit;
      Executed_Cycles : Natural) is
      Temp_Value : Value_8_Bit;
   begin
      Mem_Read_Floating_Bus (Temp_Value, Executed_Cycles);
      Temp_Value := Temp_Value and 16#7F#;
      if High_Bit then
         Read_Value := Temp_Value or 16#80#;
      else
         Read_Value := Temp_Value;
      end if;
   end Mem_Read_Floating_Bus;

   ---------------------------
   -- Next_Three_Code_Bytes --
   ---------------------------

   function Next_Three_Code_Bytes (PC : Address_16_Bit) return Value_32_Bit is
      Value : Value_32_Bit := 0;
   begin
      for I in Address_16_Bit (0) .. Address_16_Bit (2) loop
         if PC <= 16#FFFF# - I then
            Value := Value or Shift_Left (Value_32_Bit (Mem_Image (PC + I)),
                                          Integer (I * 8));
         end if;
      end loop;
      return Value;
   end Next_Three_Code_Bytes;

   --------------------
   -- Mem_Set_Paging --
   --------------------

   procedure Mem_Set_Paging
     (PC, Address : Address_16_Bit; Is_Write : Boolean;
      Write_Value : Value_8_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (Is_Write);
      Offset : constant Value_8_Bit := Value_8_Bit (Address and 16#FF#);
      Last_Mem_Mode : constant Mem_Flag_Type := Mem_Mode;
   begin
      if Offset >= 16#80# and Offset <= 16#8F# then
         declare
            Write_RAM : constant Boolean := (Offset and 1) /= 0;
         begin
            Mem_Mode := Mem_Mode and not
              (Flag_High_RAM_Bank_2 or Flag_High_RAM or Flag_High_RAM_Write);
            Last_Write_RAM := True;
            --  Note: because diags.do doesn't set switches twice!
            if Last_Write_RAM and Write_RAM then
               Mem_Mode := Mem_Mode or Flag_High_RAM_Write;
            end if;
            if (Offset and 8) = 0 then
               Mem_Mode := Mem_Mode or Flag_High_RAM_Bank_2;
            end if;
            if Shift_Right (Offset and 2, 1) = (Offset and 1) then
               Mem_Mode := Mem_Mode or Flag_High_RAM;
            end if;

            Last_Write_RAM := Write_RAM;
         end;
      elsif not Is_Apple2 then
         case Offset is
         when 16#00# =>
            Mem_Mode := Mem_Mode and not Flag_80_Store;
         when 16#01# =>
            Mem_Mode := Mem_Mode or Flag_80_Store;
         when 16#02# =>
            Mem_Mode := Mem_Mode and not Flag_Aux_Read;
         when 16#03# =>
            Mem_Mode := Mem_Mode or Flag_Aux_Read;
         when 16#04# =>
            Mem_Mode := Mem_Mode and not Flag_Aux_Write;
         when 16#05# =>
            Mem_Mode := Mem_Mode or Flag_Aux_Write;
         when 16#06# =>
            Mem_Mode := Mem_Mode and not Flag_Slot_CX_ROM;
         when 16#07# =>
            Mem_Mode := Mem_Mode or Flag_Slot_CX_ROM;
         when 16#08# =>
            Mem_Mode := Mem_Mode and not Flag_Alt_ZP;
         when 16#09# =>
            Mem_Mode := Mem_Mode or Flag_Alt_ZP;
         when 16#0A# =>
            Mem_Mode := Mem_Mode and not Flag_Slot_C3_ROM;
         when 16#0B# =>
            Mem_Mode := Mem_Mode or Flag_Slot_C3_ROM;
         when 16#54# =>
            Mem_Mode := Mem_Mode and not Flag_Page_2;
         when 16#55# =>
            Mem_Mode := Mem_Mode or Flag_Page_2;
         when 16#56# =>
            Mem_Mode := Mem_Mode and not Flag_Hi_Res;
         when 16#57# =>
            Mem_Mode := Mem_Mode or Flag_Hi_Res;
         when 16#71# | 16#73# =>
            --  16#71# - extended memory aux page number
            --  16#73# - RAMWorks III set aux page number
            if Write_Value < RAM_Works_Num_Pages then
               RAM_Works_Active_Bank := RAM_Works_Bank_Range (Write_Value);
               Mem_Aux := RAM_Works_Pages (RAM_Works_Active_Bank)'Access;
               Mem_Update_Paging (False, False);
            end if;
         when others =>
            null;
         end case;
      end if;

      --  If the emulated program has just updated the memory write mode and
      --  is about to update the memory read mode, hold off on any processing
      --  until it does so.

      declare
         Next_Three_Bytes : constant Value_32_Bit :=
           Next_Three_Code_Bytes (PC) and 16#00FFFEFF#;
      begin
         if (Offset >= 4 and Offset <= 5 and Next_Three_Bytes = 16#00C0028D#)
           or (Offset >= 16#80# and Offset <= 16#8F# and PC < 16#C000# and
                 (Next_Three_Bytes = 16#00C0048D# or
                      Next_Three_Bytes = 16#00C0028D#))
         then
            Mode_Changing := True;
            Mem_Read_Floating_Bus (True, Read_Value, Cycles_Left);
            return;
         end if;
      end;

      --  If the memory paging mode has changed, update our memory images
      --  and write tables.

      if Last_Mem_Mode /= Mem_Mode or Mode_Changing then
         Mode_Changing := False;

         if (Last_Mem_Mode and Flag_Slot_CX_ROM) /=
           (Mem_Mode and Flag_Slot_CX_ROM)
         then
            if Mode_Slot_CX_ROM then
               --  Disable Internal ROM
               --    Similar to $CFFF access
               --    None of the peripheral cards can be driving the bus, so
               --      use the null ROM.
               Cx_ROM_Peripheral (16#0800# .. 16#0FFF#) := (others => 0);
               Mem_Image (16#C800# .. 16#CFFF#) := (others => 0);
               Expansion_ROM_Type := ROM_Null;
               Peripheral_ROM_Slot := 0;
            else
               --  Enable Internal ROM
               Mem_Image (16#C800# .. 16#CFFF#) :=
                 Cx_ROM_Peripheral (16#0800# .. 16#0FFF#);
               Expansion_ROM_Type := ROM_Internal;
               Peripheral_ROM_Slot := 0;
            end if;
         end if;

         Mem_Update_Paging (False, False);
      end if;

      if Offset <= 1 or (Offset >= 16#54# and Offset <= 16#57#) then
         Video_Set_Mode (Address);
      end if;

      Mem_Read_Floating_Bus (Read_Value, Cycles_Left);
   end Mem_Set_Paging;

   ---------------
   -- Mem_Reset --
   ---------------

   procedure Mem_Reset is
   begin
      --  Initialize the paging tables
      Mem_Shadow := (others => null);
      Mem_Write := (others => null);

      --  Initialize the RAM images
      Mem_Aux.all := (others => 0);
      Mem_Main := (others => 0);

      if Mem_Init_Pattern = Pattern_FF_FF_00_00 then
         declare
            I : Address_16_Bit := 0;
         begin
            while I < Address_16_Bit (16#C000#) loop
               Mem_Main (I) := 16#FF#;
               Mem_Main (I + 1) := 16#FF#;
               I := I + 4;
            end loop;
         end;
      end if;

      --  Initialize paging, filling in the 64k memory image
      Reset_Paging (True);

      --  Initialize & reset the cpu
      --    Do this after ROM has been copied back to mem, so that PC is
      --      correctly init'ed from 6502's reset vector
      CPU_Initialize;
   end Mem_Reset;

   -------------------------
   -- Mem_Reset_Paging --
   -------------------------

   procedure Mem_Reset_Paging is
   begin
      Reset_Paging (False);
   end Mem_Reset_Paging;

   --------------------------
   -- Mem_Read_Random_Data --
   --------------------------

   function Mem_Read_Random_Data
     (High_Bit : Boolean) return Value_8_Bit is
      Ret_Values : constant Mem_Range (0 .. 15) :=
        (16#00#, 16#2D#, 16#2D#, 16#30#, 16#30#, 16#32#, 16#32#, 16#34#,
         16#35#, 16#39#, 16#43#, 16#43#, 16#43#, 16#60#, 16#7F#, 16#7F#);
      Value : Value_8_Bit := Random_Byte.Random (Random_Generator);
   begin
      if Value <= 170 then
         if High_Bit then
            return 16#A0#;
         else
            return 16#20#;
         end if;
      else
         Value := Ret_Values (Address_16_Bit (Value and 15));
         if High_Bit then
            return Value or 16#80#;
         else
            return Value;
         end if;
      end if;
   end Mem_Read_Random_Data;

   ----------------------
   -- Mem_Get_Snapshot --
   ----------------------

   procedure Mem_Get_Snapshot (Snapshot : in out Snapshot_Base_Memory) is
   begin
      Snapshot.Mem_Mode := Mem_Mode;
      Snapshot.Last_Write_RAM := Last_Write_RAM;

      for I in Address_16_Bit (0) .. Address_16_Bit (16#FF#) loop
         declare
            Offset : constant Address_16_Bit := Shift_Left (I, 8);
            Copy_Range : Mem_Range_Access (0 .. 255);
         begin
            Mem_Get_Main_Range (Offset, 256, Copy_Range);
            Snapshot.Mem_Main (Offset .. Offset + 255) := Copy_Range.all;
            Mem_Get_Aux_Range (Offset, 256, Copy_Range);
            Snapshot.Mem_Aux (Offset .. Offset + 255) := Copy_Range.all;
         end;
      end loop;
   end Mem_Get_Snapshot;

   ----------------------
   -- Mem_Set_Snapshot --
   ----------------------

   procedure Mem_Set_Snapshot (Snapshot : Snapshot_Base_Memory) is
   begin
      Mem_Mode := Snapshot.Mem_Mode;
      Last_Write_RAM := Snapshot.Last_Write_RAM;

      Mem_Main := Snapshot.Mem_Main;
      Mem_Aux.all := Snapshot.Mem_Aux;

      Mode_Changing := False;
   end Mem_Set_Snapshot;

end Apple2.Memory;
