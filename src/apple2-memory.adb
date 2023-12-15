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

package body Apple2.Memory with
  SPARK_Mode
is

   -----------------
   -- Mem_IO_Read --
   -----------------

   overriding procedure Mem_IO_Read
     (C           : in out Computer; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural)
   is
   begin
      --  Handle the non-I/O cases first
      if (Address and 16#F000#) = 16#D000# and not Mode_High_RAM_Bank_2 (C)
      then
         Read_Value :=
           Mem_Read
             (Mem, C.Mem_Read_Bank (Unsigned_8 (Shift_Right (Address, 8))),
              Address - 16#1000#);
         --  Read $Cxxx region of RAM at $Dxxx
      elsif (Address and 16#F000#) /= 16#C000# then
         Read_Value :=
           Mem_Read
             (Mem, C.Mem_Read_Bank (Unsigned_8 (Shift_Right (Address, 8))),
              Address);
      else
         Mem_IO_Read_Cxxx (C, Mem, Address, Read_Value, Cycles_Left);
      end if;
   end Mem_IO_Read;

   ------------------
   -- Mem_IO_Write --
   ------------------

   overriding procedure Mem_IO_Write
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address : Unsigned_16; Write_Value : Unsigned_8; Cycles_Left : Natural)
   is
   begin
      --  Handle the non-I/O cases first
      if Address >= 16#D000# and not Mode_High_RAM_Write (C) then
         null;
         --  Ignore writes to ROM area unless High_RAM_Write is set
      elsif (Address and 16#F000#) = 16#D000# and not Mode_High_RAM_Bank_2 (C)
      then
         Mem_Write
           (C, Mem, C.Mem_Write_Bank (Unsigned_8 (Shift_Right (Address, 8))),
            Address - 16#1000#, Write_Value);
         --  Write to $Cxxx region of RAM at $Dxxx
      elsif (Address and 16#F000#) /= 16#C000# then
         Mem_Write
           (C, Mem, C.Mem_Write_Bank (Unsigned_8 (Shift_Right (Address, 8))),
            Address, Write_Value);
      else
         Mem_IO_Write_Cxxx (C, Mem, Address, Write_Value, Cycles_Left);
      end if;
   end Mem_IO_Write;

   procedure IO_Read_C00x
     (C : in out Computer; Read_Value : out Unsigned_8) with
     Inline;
   --  Keyboard

   procedure IO_Write_C00x
     (C           : in out Computer; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Write_Value : Unsigned_8;
      Cycles_Left :        Natural) with
     Inline;
   --  Memory / Video

   procedure IO_Read_C01x
     (C : in out Computer; Address : Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural) with
     Inline;
   --  Memory / Video

   procedure IO_Write_C01x (C : in out Computer) with
     Inline;
   --  Keyboard

   procedure IO_Read_C03x
     (C           : in out Computer; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural) with
     Inline;
   --  Speaker

   procedure IO_Write_C03x (C : in out Computer; Cycles_Left : Natural) with
     Inline;
   --  Speaker

   procedure IO_Read_C05x
     (C           : in out Computer; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural) with
     Inline;
   --  Video

   procedure IO_Write_C05x
     (C           : in out Computer; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Write_Value : Unsigned_8;
      Cycles_Left :        Natural) with
     Inline;
   --  Video / Memory

   procedure IO_Read_C06x
     (C : in out Computer; Mem : not null access constant RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural) with
     Inline;
   --  Joystick

   procedure IO_Read_C07x
     (C : in out Computer; Mem : not null access constant RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural) with
     Inline;
   --  Joystick / Video

   procedure IO_Write_C07x
     (C           : in out Computer; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Write_Value : Unsigned_8;
      Cycles_Left :        Natural) with
     Inline;
   --  Joystick / RAMWorks

   procedure IO_Read_C1xx
     (C           : in out Computer; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural) with
     Inline;
   --  Expansion ROM select

   function Mode_Alt_ZP (C : Computer) return Boolean with
     Inline;
   --  Memory mode is alt zero page

   function Mode_Aux_Read (C : Computer) return Boolean with
     Inline;
   --  Memory mode is aux read

   function Mode_Aux_Write (C : Computer) return Boolean with
     Inline;
   --  Memory mode is aux write

   function Mode_High_RAM (C : Computer) return Boolean with
     Inline;
   --  Memory mode is high RAM

   function Mode_Hi_Res (C : Computer) return Boolean with
     Inline;
   --  Memory mode is hi-res

   function Mode_Page_2 (C : Computer) return Boolean with
     Inline;
   --  Memory mode is page 2

   function Mode_Slot_C3_ROM (C : Computer) return Boolean with
     Inline;
   --  Memory mode is slot C3 ROM

   function Mode_Slot_CX_ROM (C : Computer) return Boolean with
     Inline;
   --  Memory mode is slot CX ROM

   procedure Mem_Set_Paging
     (C          : in out Computer; Mem : not null access RAM_All_Banks;
      Address    :        Unsigned_16; Write_Value : Unsigned_8;
      Read_Value :    out Unsigned_8; Cycles_Left : Natural);
   --  Set or get memory address associated with paging

   procedure Mem_Update_Paging (C : in out Computer);
   --  Update the paging tables based on the new paging switch values

   procedure Mem_Check_Paging
     (C : Computer; Address : Unsigned_16; Read_Value : out Unsigned_8);
   --  Read byte containing paging mode and keyboard scan code

   ------------------
   -- IO_Read_Null --
   ------------------

   procedure IO_Read_Null
     (C : in out Computer; Mem : not null access constant RAM_All_Banks;
      Read_Value :    out Unsigned_8; Cycles_Left : Natural)
   is
   begin
      Mem_Read_Floating_Bus (C, Mem, Read_Value, Cycles_Left);
   end IO_Read_Null;

   -------------------
   -- IO_Write_Null --
   -------------------

   procedure IO_Write_Null (C : in out Computer; Cycles_Left : Natural) is
   begin
      --  Note: original code only counted cycles for reads, not writes
      CPU_Calc_Cycles (C, Cycles_Left);
   end IO_Write_Null;

   ------------------
   -- IO_Read_C00x --
   ------------------

   procedure IO_Read_C00x (C : in out Computer; Read_Value : out Unsigned_8) is
   begin
      Keyb_Read_Data (Apple2_Base (C), Read_Value);
   end IO_Read_C00x;

   -------------------
   -- IO_Write_C00x --
   -------------------

   procedure IO_Write_C00x
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address : Unsigned_16; Write_Value : Unsigned_8; Cycles_Left : Natural)
   is
      Ignore : Unsigned_8;  --  ignore read value
   begin
      if (Address and 16#0F#) <= 16#0B# then
         Mem_Set_Paging (C, Mem, Address, Write_Value, Ignore, Cycles_Left);
      else
         Video_Set_Mode (Apple2_Base (C), Address);
      end if;
   end IO_Write_C00x;

   ------------------
   -- IO_Read_C01x --
   ------------------

   procedure IO_Read_C01x
     (C : in out Computer; Address : Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural)
   is
      Offset : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      case Offset is
         when 16#00# =>
            Keyb_Read_Flag (Apple2_Base (C), Read_Value);

         when 16#01# .. 16#08# | 16#0C# .. 16#0D# =>
            Mem_Check_Paging (C, Address, Read_Value);

         when 16#09# =>
            Video_Check_VBL (Apple2_Base (C), Read_Value, Cycles_Left);

         when 16#0A# .. 16#0B# | 16#0E# .. 16#0F# =>
            Video_Check_Mode
              (Apple2_Base (C), Address, Read_Value, Cycles_Left);

      end case;
   end IO_Read_C01x;

   -------------------
   -- IO_Write_C01x --
   -------------------

   procedure IO_Write_C01x (C : in out Computer) is
      Ignore : Unsigned_8;  --  ignore read value
   begin
      Keyb_Read_Flag (Apple2_Base (C), Ignore);
   end IO_Write_C01x;

   ------------------
   -- IO_Read_C03x --
   ------------------

   procedure IO_Read_C03x
     (C : in out Computer; Read_Value : out Unsigned_8; Cycles_Left : Natural)
   is
   begin
      Read_Value := 0;
      Spkr_Toggle (Apple2_Base (C), Cycles_Left);
   end IO_Read_C03x;

   -------------------
   -- IO_Write_C03x --
   -------------------

   procedure IO_Write_C03x (C : in out Computer; Cycles_Left : Natural) is
   begin
      Spkr_Toggle (Apple2_Base (C), Cycles_Left);
   end IO_Write_C03x;

   ------------------
   -- IO_Read_C05x --
   ------------------

   procedure IO_Read_C05x
     (C           : in out Computer; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural)
   is
      Offset : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      case Offset is
         when 16#00# .. 16#03# | 16#0E# .. 16#0F# =>
            Video_Set_Mode (Apple2_Base (C), Address);
            Mem_Read_Floating_Bus (C, Mem, Read_Value, Cycles_Left);

         when 16#04# .. 16#07# =>
            Mem_Set_Paging (C, Mem, Address, 0, Read_Value, Cycles_Left);

         when 16#08# .. 16#0D# =>
            IO_Read_Null (C, Mem, Read_Value, Cycles_Left);

      end case;
   end IO_Read_C05x;

   -------------------
   -- IO_Write_C05x --
   -------------------

   procedure IO_Write_C05x
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address : Unsigned_16; Write_Value : Unsigned_8; Cycles_Left : Natural)
   is
      Ignore : Unsigned_8;  --  ignore read value
      Offset : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      case Offset is
         when 16#00# .. 16#03# | 16#0E# .. 16#0F# =>
            Video_Set_Mode (Apple2_Base (C), Address);

         when 16#04# .. 16#07# =>
            Mem_Set_Paging (C, Mem, Address, Write_Value, Ignore, Cycles_Left);

         when 16#08# .. 16#0D# =>
            IO_Write_Null (C, Cycles_Left);

      end case;
   end IO_Write_C05x;

   ------------------
   -- IO_Read_C06x --
   ------------------

   procedure IO_Read_C06x
     (C : in out Computer; Mem : not null access constant RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural)
   is
      Offset : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      case Offset is
         when 16#00# | 16#08# .. 16#0F# =>
            IO_Read_Null (C, Mem, Read_Value, Cycles_Left);

         when 16#01# .. 16#03# =>
            Joy_Read_Button
              (Apple2_Base (C), Address, Read_Value, Cycles_Left);

         when 16#04# .. 16#07# =>
            Joy_Read_Position
              (Apple2_Base (C), Address, Read_Value, Cycles_Left);

      end case;
   end IO_Read_C06x;

   ------------------
   -- IO_Read_C07x --
   ------------------

   procedure IO_Read_C07x
     (C : in out Computer; Mem : not null access constant RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural)
   is
      Offset : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      case Offset is
         when 16#00# =>
            Joy_Reset_Position (Apple2_Base (C), Cycles_Left);
            Mem_Read_Floating_Bus (C, Mem, Read_Value, Cycles_Left);

         when 16#01# .. 16#0E# =>
            IO_Read_Null (C, Mem, Read_Value, Cycles_Left);

         when 16#0F# =>
            Video_Check_Mode
              (Apple2_Base (C), Address, Read_Value, Cycles_Left);

      end case;
   end IO_Read_C07x;

   -------------------
   -- IO_Write_C07x --
   -------------------

   procedure IO_Write_C07x
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address : Unsigned_16; Write_Value : Unsigned_8; Cycles_Left : Natural)
   is
      Ignore : Unsigned_8;  --  ignore read value
      Offset : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      case Offset is
         when 16#00# =>
            Joy_Reset_Position (Apple2_Base (C), Cycles_Left);

         when 16#01# | 16#03# =>
            Mem_Set_Paging (C, Mem, Address, Write_Value, Ignore, Cycles_Left);

         when 16#02# | 16#04# .. 16#0F# =>
            IO_Write_Null (C, Cycles_Left);

      end case;
   end IO_Write_C07x;

   ----------------------
   -- Mem_IO_Read_Cxxx --
   ----------------------

   procedure Mem_IO_Read_Cxxx
     (C           : in out Computer; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural)
   is
   begin
      --  The configuration is now hardcoded to enable SPARK static analysis.
      --  Hopefully it's not much slower than the original function pointers.
      case Unsigned_8 (Shift_Right (Address, 4) and 16#FF#) is
         when 16#00# =>
            IO_Read_C00x (C, Read_Value);
         when 16#01# =>
            IO_Read_C01x (C, Address, Read_Value, Cycles_Left);
         when 16#02# =>
            IO_Read_Null (C, Mem, Read_Value, Cycles_Left);
         when 16#03# =>
            IO_Read_C03x (C, Read_Value, Cycles_Left);
         when 16#04# =>
            IO_Read_Null (C, Mem, Read_Value, Cycles_Left);
         when 16#05# =>
            IO_Read_C05x (C, Mem, Address, Read_Value, Cycles_Left);
         when 16#06# =>
            IO_Read_C06x (C, Mem, Address, Read_Value, Cycles_Left);
         when 16#07# =>
            IO_Read_C07x (C, Mem, Address, Read_Value, Cycles_Left);
         when 16#08# =>
            --  slot 0
            Mem_Set_Paging (C, Mem, Address, 0, Read_Value, Cycles_Left);
         when 16#09# =>
            --  slot 1 (parallel printer card)
            Print_Status (Apple2_Base (C), Read_Value);
         when 16#0A# =>
            --  slot 2 (super serial card)
            SSC_Read (Apple2_Base (C), Address, Read_Value, Cycles_Left);
         when 16#0B# =>
            --  slot 3 (no card)
            IO_Read_Null (C, Mem, Read_Value, Cycles_Left);
         when 16#0C# =>
            --  slot 4 (Mockingboard or mouse)
            Phasor_IO (Apple2_Base (C), Mem, Address);
            Mem_Read_Floating_Bus (C, Mem, Read_Value, Cycles_Left);
         when 16#0D# =>
            --  slot 5 (Phasor sound card)
            Phasor_IO (Apple2_Base (C), Mem, Address);
            Mem_Read_Floating_Bus (C, Mem, Read_Value, Cycles_Left);
         when 16#0E# =>
            --  slot 6 (Disk ][)
            Disk_IO_Read
              (Apple2_Base (C), Mem, Address, Read_Value, Cycles_Left);
         when 16#0F# =>
            --  slot 7 (no card)
            IO_Read_Null (C, Mem, Read_Value, Cycles_Left);
         when 16#40# .. 16#5F# =>
            --  slots 4 and 5 $C400 .. $C5FF space (Mockingboard / Phasor)
            MB_Read (Apple2_Base (C), Mem, Address, Read_Value, Cycles_Left);
         when others =>
            IO_Read_C1xx (C, Mem, Address, Read_Value, Cycles_Left);
      end case;
   end Mem_IO_Read_Cxxx;

   -----------------------
   -- Mem_IO_Write_Cxxx --
   -----------------------

   procedure Mem_IO_Write_Cxxx
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address : Unsigned_16; Write_Value : Unsigned_8; Cycles_Left : Natural)
   is
      Index  : constant Unsigned_8 :=
        Unsigned_8 (Shift_Right (Address, 4) and 16#FF#);
      Ignore : Unsigned_8;  --  ignore read value
   begin
      --  The configuration is now hardcoded to enable SPARK static analysis.
      --  Hopefully it's not much slower than the original function pointers.
      case Index is
         when 16#00# =>
            IO_Write_C00x (C, Mem, Address, Write_Value, Cycles_Left);
         when 16#01# =>
            IO_Write_C01x (C);
         when 16#02# =>
            IO_Write_Null (C, Cycles_Left);
         when 16#03# =>
            IO_Write_C03x (C, Cycles_Left);
         when 16#04# =>
            IO_Write_Null (C, Cycles_Left);
         when 16#05# =>
            IO_Write_C05x (C, Mem, Address, Write_Value, Cycles_Left);
         when 16#06# =>
            IO_Write_Null (C, Cycles_Left);
         when 16#07# =>
            IO_Write_C07x (C, Mem, Address, Write_Value, Cycles_Left);
         when 16#08# =>
            --  slot 0
            Mem_Set_Paging (C, Mem, Address, Write_Value, Ignore, Cycles_Left);
         when 16#09# =>
            --  slot 1 (parallel printer card)
            Print_Transmit (Apple2_Base (C), Write_Value);
         when 16#0A# =>
            --  slot 2 (super serial card)
            SSC_Write (Apple2_Base (C), Address, Write_Value, Cycles_Left);
         when 16#0B# =>
            --  slot 3 (no card)
            IO_Write_Null (C, Cycles_Left);
         when 16#0C# =>
            --  slot 4 (Mockingboard or mouse)
            Phasor_IO (Apple2_Base (C), Mem, Address);
            CPU_Calc_Cycles (C, Cycles_Left);
         when 16#0D# =>
            --  slot 5 (Phasor sound card)
            Phasor_IO (Apple2_Base (C), Mem, Address);
            CPU_Calc_Cycles (C, Cycles_Left);
         when 16#0E# =>
            --  slot 6 (Disk ][)
            Disk_IO_Write
              (Apple2_Base (C), Mem, Address, Write_Value, Cycles_Left);
         when 16#0F# =>
            --  slot 7 (no card)
            IO_Write_Null (C, Cycles_Left);
         when 16#40# .. 16#5F# =>
            --  slots 4 and 5 $Cxxx space (Mockingboard / Phasor)
            MB_Write (Apple2_Base (C), Mem, Address, Write_Value, Cycles_Left);
         when others =>
            IO_Write_Null (C, Cycles_Left);
      end case;
   end Mem_IO_Write_Cxxx;

   -------------------
   -- Mode_80_Store --
   -------------------

   function Mode_80_Store (C : Computer) return Boolean is
   begin
      return (C.Mem_Mode and Flag_80_Store) /= 0;
   end Mode_80_Store;

   -----------------
   -- Mode_Alt_ZP --
   -----------------

   function Mode_Alt_ZP (C : Computer) return Boolean is
   begin
      return (C.Mem_Mode and Flag_Alt_ZP) /= 0;
   end Mode_Alt_ZP;

   -------------------
   -- Mode_Aux_Read --
   -------------------

   function Mode_Aux_Read (C : Computer) return Boolean is
   begin
      return (C.Mem_Mode and Flag_Aux_Read) /= 0;
   end Mode_Aux_Read;

   --------------------
   -- Mode_Aux_Write --
   --------------------

   function Mode_Aux_Write (C : Computer) return Boolean is
   begin
      return (C.Mem_Mode and Flag_Aux_Write) /= 0;
   end Mode_Aux_Write;

   --------------------------
   -- Mode_High_RAM_Bank_2 --
   --------------------------

   function Mode_High_RAM_Bank_2 (C : Computer) return Boolean is
   begin
      return (C.Mem_Mode and Flag_High_RAM_Bank_2) /= 0;
   end Mode_High_RAM_Bank_2;

   -------------------
   -- Mode_High_RAM --
   -------------------

   function Mode_High_RAM (C : Computer) return Boolean is
   begin
      return (C.Mem_Mode and Flag_High_RAM) /= 0;
   end Mode_High_RAM;

   -----------------
   -- Mode_Hi_Res --
   -----------------

   function Mode_Hi_Res (C : Computer) return Boolean is
   begin
      return (C.Mem_Mode and Flag_Hi_Res) /= 0;
   end Mode_Hi_Res;

   -----------------
   -- Mode_Page_2 --
   -----------------

   function Mode_Page_2 (C : Computer) return Boolean is
   begin
      return (C.Mem_Mode and Flag_Page_2) /= 0;
   end Mode_Page_2;

   ----------------------
   -- Mode_Slot_C3_ROM --
   ----------------------

   function Mode_Slot_C3_ROM (C : Computer) return Boolean is
   begin
      return (C.Mem_Mode and Flag_Slot_C3_ROM) /= 0;
   end Mode_Slot_C3_ROM;

   ----------------------
   -- Mode_Slot_CX_ROM --
   ----------------------

   function Mode_Slot_CX_ROM (C : Computer) return Boolean is
   begin
      return (C.Mem_Mode and Flag_Slot_CX_ROM) /= 0;
   end Mode_Slot_CX_ROM;

   -------------------------
   -- Mode_High_RAM_Write --
   -------------------------

   function Mode_High_RAM_Write (C : Computer) return Boolean is
   begin
      return (C.Mem_Mode and Flag_High_RAM_Write) /= 0;
   end Mode_High_RAM_Write;

   ------------------
   -- IO_Read_C1xx --
   ------------------

   --  Enabling expansion ROM ($C800..$CFFF]:
   --  . Enable if: Enable1 && Enable2
   --  . Enable1 = I/O SELECT' (6502 accesses $Csxx)
   --    - Reset when 6502 accesses $CFFF
   --  . Enable2 = I/O STROBE' (6502 accesses [$C800..$CFFF])

   procedure IO_Read_C1xx
     (C           : in out Computer; Mem : not null access RAM_All_Banks;
      Address     :        Unsigned_16; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural)
   is
      IO_Strobe : Boolean := False;
      Slot      : Slot_Range;
   begin
      if Address = 16#CFFF# then
         --  Disable expansion ROM at [$C800..$CFFF]
         --  SSC will disable on an access to $CFxx, but ROM only writes to
         --  $CFFF, so it doesn't matter
         C.IO_Select_Slot         := 0;
         C.IO_Select_Internal_ROM := False;

         if Mode_Slot_CX_ROM (C) then
            --  unset Slot_CX_ROM ensures that internal rom stays switched in
            C.Expansion_ROM := ROM_None;
         end if;
         --  IO_Select_Slot is now unset, so ROM won't be switched back in
      end if;

      if Is_Apple2 (C) or Mode_Slot_CX_ROM (C) then
         if Address >= 16#C100# and Address <= 16#C7FF# then
            Slot := Slot_Range (Shift_Right (Address, 8) and 7);
            if Slot = 2 then
               --  Note: add any other slots with expansion ROMs below
               --  (you will have to copy the active ROM into bank 2)
               C.IO_Select_Slot := Slot;
            elsif not Mode_Slot_C3_ROM (C) then
               C.IO_Select_Internal_ROM := True;
               --  Slot 3 & Internal ROM
            end if;
         elsif Address >= 16#C800# and Address <= 16#CFFF# then
            IO_Strobe := True;
         end if;

         if C.IO_Select_Slot /= 0 and IO_Strobe then
            --  Enable Peripheral Expansion ROM
            C.Mem_Read_Bank (16#C8# .. 16#CF#) :=
              (others => ROM_Bank_Peripheral);
            C.Expansion_ROM                    := ROM_Peripheral;
         elsif C.IO_Select_Internal_ROM and IO_Strobe then
            --  Enable Internal ROM (get this for PR#3)
            C.Mem_Read_Bank (16#C8# .. 16#CF#) :=
              (others => ROM_Bank_Internal);
            C.Expansion_ROM                    := ROM_Internal;
         end if;
      end if;

      if not Is_Apple2 (C) and not Mode_Slot_CX_ROM (C) then
         --  not Mode_Slot_C3_ROM = Internal ROM: $C300-C3FF
         --  not Mode_Slot_CX_ROM = Internal ROM: $C100-CFFF

         if Address >= 16#C100# and Address <= 16#C7FF# then
            --  Don't care about state of Slot_C3_ROM
            C.IO_Select_Internal_ROM := True;
         elsif Address >= 16#C800# and Address <= 16#CFFF# then
            IO_Strobe := True;
         end if;

         if not Mode_Slot_CX_ROM (C) and C.IO_Select_Internal_ROM and IO_Strobe
         then
            --  Enable Internal ROM
            C.Mem_Read_Bank (16#C8# .. 16#CF#) :=
              (others => ROM_Bank_Internal);
            C.Expansion_ROM                    := ROM_Internal;
         end if;
      end if;

      if C.Expansion_ROM = ROM_None and Address >= 16#C800# then
         IO_Read_Null (C, Mem, Read_Value, Cycles_Left);
      else
         Read_Value :=
           Mem_Read
             (Mem, C.Mem_Read_Bank (Unsigned_8 (Shift_Right (Address, 8))),
              Address);
      end if;
   end IO_Read_C1xx;

   ------------------
   -- Mem_Get_Mode --
   ------------------

   function Mem_Get_Mode (C : Computer) return Mem_Mode_Flags is
   begin
      return C.Mem_Mode;
   end Mem_Get_Mode;

   ------------------
   -- Mem_Set_Mode --
   ------------------

   procedure Mem_Set_Mode (C : in out Computer; Mode : Mem_Mode_Flags) is
   begin
      C.Mem_Mode := Mode;
   end Mem_Set_Mode;

   ----------------------
   -- Mem_Reset_Paging --
   ----------------------

   procedure Mem_Reset_Paging (C : in out Computer) is
   begin
      C.Mem_Mode :=
        Flag_High_RAM_Bank_2 or Flag_Slot_CX_ROM or Flag_High_RAM_Write;

      Mem_Update_Paging (C);
   end Mem_Reset_Paging;

   -----------------------
   -- Mem_Update_Paging --
   -----------------------

   procedure Mem_Update_Paging (C : in out Computer) is
   begin
      for I in Unsigned_8 (16#00#) .. Unsigned_8 (16#01#) loop
         if Mode_Alt_ZP (C) then
            C.Mem_Read_Bank (I)  := C.RAM_Active_Bank;
            C.Mem_Write_Bank (I) := C.RAM_Active_Bank;
         else
            C.Mem_Read_Bank (I)  := RAM_Bank_Main;
            C.Mem_Write_Bank (I) := RAM_Bank_Main;
         end if;
      end loop;

      for I in Unsigned_8 (16#02#) .. Unsigned_8 (16#BF#) loop
         if Mode_Aux_Read (C) then
            C.Mem_Read_Bank (I) := C.RAM_Active_Bank;
         else
            C.Mem_Read_Bank (I) := RAM_Bank_Main;
         end if;

         if Mode_Aux_Write (C) then
            C.Mem_Write_Bank (I) := C.RAM_Active_Bank;
         else
            C.Mem_Write_Bank (I) := RAM_Bank_Main;
         end if;
      end loop;

      for I in Unsigned_8 (16#C1#) .. Unsigned_8 (16#CF#) loop
         if Mode_Slot_CX_ROM (C) and (I /= 16#C3# or Mode_Slot_C3_ROM (C)) then
            C.Mem_Read_Bank (I) := ROM_Bank_Peripheral;
            --  C100..CFFF - SSC/Disk ][/etc
         else
            C.Mem_Read_Bank (I) := ROM_Bank_Internal;
            --  C100..CFFF - Internal ROM
         end if;
      end loop;

      for I in Unsigned_8 (16#D0#) .. Unsigned_8 (16#FF#) loop
         --  Note: offset from $Dxxx to $Cxxx handled by Mem_IO_Read / Write
         if Mode_High_RAM (C) then
            if Mode_Alt_ZP (C) then
               C.Mem_Read_Bank (I) := C.RAM_Active_Bank;
            else
               C.Mem_Read_Bank (I) := RAM_Bank_Main;
            end if;
         else
            C.Mem_Read_Bank (I) := ROM_Bank_Internal;
         end if;

         if Mode_High_RAM_Write (C) then
            if Mode_Alt_ZP (C) then
               C.Mem_Write_Bank (I) := C.RAM_Active_Bank;
            else
               C.Mem_Write_Bank (I) := RAM_Bank_Main;
            end if;
         end if;
      end loop;

      if Mode_80_Store (C) then
         for I in Unsigned_8 (16#04#) .. Unsigned_8 (16#07#) loop
            if Mode_Page_2 (C) then
               C.Mem_Read_Bank (I)  := C.RAM_Active_Bank;
               C.Mem_Write_Bank (I) := C.RAM_Active_Bank;
            else
               C.Mem_Read_Bank (I)  := RAM_Bank_Main;
               C.Mem_Write_Bank (I) := RAM_Bank_Main;
            end if;
         end loop;

         if Mode_Hi_Res (C) then
            for I in Unsigned_8 (16#20#) .. Unsigned_8 (16#3F#) loop
               if Mode_Page_2 (C) then
                  C.Mem_Read_Bank (I)  := C.RAM_Active_Bank;
                  C.Mem_Write_Bank (I) := C.RAM_Active_Bank;
               else
                  C.Mem_Read_Bank (I)  := RAM_Bank_Main;
                  C.Mem_Write_Bank (I) := RAM_Bank_Main;
               end if;
            end loop;
         end if;
      end if;
   end Mem_Update_Paging;

   ----------------------
   -- Mem_Check_Paging --
   ----------------------

   procedure Mem_Check_Paging
     (C : Computer; Address : Unsigned_16; Read_Value : out Unsigned_8)
   is
      Mode_Value : Boolean;
      Key_Value  : Unsigned_8;
   begin
      --  TODO: >= Apple 2e only?
      case Unsigned_8 (Address and 16#FF#) is
         when 16#11# =>
            Mode_Value := Mode_High_RAM_Bank_2 (C);
         when 16#12# =>
            Mode_Value := Mode_High_RAM (C);
         when 16#13# =>
            Mode_Value := Mode_Aux_Read (C);
         when 16#14# =>
            Mode_Value := Mode_Aux_Write (C);
         when 16#15# =>
            Mode_Value := not Mode_Slot_CX_ROM (C);
         when 16#16# =>
            Mode_Value := Mode_Alt_ZP (C);
         when 16#17# =>
            Mode_Value := Mode_Slot_C3_ROM (C);
         when 16#18# =>
            Mode_Value := Mode_80_Store (C);
         when 16#1C# =>
            Mode_Value := Mode_Page_2 (C);
         when 16#1D# =>
            Mode_Value := Mode_Hi_Res (C);
         when others =>
            Mode_Value := False;
      end case;

      Key_Value := Keyb_Get_Keycode (Apple2_Base (C));
      if Mode_Value then
         Read_Value := Key_Value or 16#80#;
      else
         Read_Value := Key_Value;
      end if;
   end Mem_Check_Paging;

   -----------------
   -- Init_Apple2 --
   -----------------

   procedure Init_Apple2
     (C : in out Computer; Mem : not null access RAM_All_Banks)
   is
   begin
      case C.Settings.Model is
         when Apple_2 =>
            Mem (16#01_D000# .. 16#01_FFFF#) := Apple_2_ROM;
            --  12KB ROM in second 64KB bank

         when Apple_2_Plus =>
            Mem (16#01_D000# .. 16#01_FFFF#) := Apple_2_Plus_ROM;
            --  12KB ROM in second 64KB bank

         when Apple_2e =>
            Mem (16#01_C000# .. 16#01_FFFF#) := Apple_2e_ROM;
            --  16KB ROM in second 64KB bank

         when others =>
            Mem (16#01_C000# .. 16#01_FFFF#) := Apple_2e_Enhanced_ROM;
            --  16KB ROM in second 64KB bank
      end case;

      Mem (16#02_C100# .. 16#02_C1FF#) := Printer_ROM;
      --  $C1xx : Parallel printer f/w in bank 2

      Mem (16#02_C200# .. 16#02_C2FF#) :=
        SSC_ROM (SSC_Slot_Firmware_Offset .. SSC_Slot_Firmware_Offset + 255);
      --  $C2xx : SSC slot f/w (256 byte section of 2KB ROM)

      Mem (16#02_C600# .. 16#02_C6FF#) := Disk_ROM;
      --  $C600 : Disk ][ f/w
      Mem (16#02_C64C#)                := 16#A9#;
      Mem (16#02_C64D#)                := 16#00#;
      Mem (16#02_C64E#)                := 16#EA#;
      --  HACK! REMOVE A WAIT ROUTINE FROM THE DISK CONTROLLER'S FIRMWARE

      Mem (16#02_C800# .. 16#02_CFFF#) := SSC_ROM;
      --  2KB SSC f/w at $C800 in bank 2 (TODO: copy other ROMs as needed)

      Mem_Reset (C, Mem);
   end Init_Apple2;

   ---------------------------
   -- Mem_Read_Floating_Bus --
   ---------------------------

   procedure Mem_Read_Floating_Bus
     (C : in out Computer; Mem : not null access constant RAM_All_Banks;
      Read_Value :    out Unsigned_8; Executed_Cycles : Natural)
   is
      VBL_Bar_Ignore : Boolean;
      Bank           : RAM_Bank_Index;
      Address        : Unsigned_16;
   begin
      Video_Get_Scanner_Address
        (Apple2_Base (C), VBL_Bar_Ignore, Bank, Address, Executed_Cycles);
      Read_Value := Mem_Read (Mem, Bank, Address);
   end Mem_Read_Floating_Bus;

   --------------------------------------
   -- Mem_Read_Floating_Bus (high bit) --
   --------------------------------------

   procedure Mem_Read_Floating_Bus
     (C : in out Computer; Mem : not null access constant RAM_All_Banks;
      High_Bit        :        Boolean; Read_Value : out Unsigned_8;
      Executed_Cycles :        Natural)
   is
      Temp_Value : Unsigned_8;
   begin
      Mem_Read_Floating_Bus (C, Mem, Temp_Value, Executed_Cycles);
      Temp_Value := Temp_Value and 16#7F#;
      if High_Bit then
         Read_Value := Temp_Value or 16#80#;
      else
         Read_Value := Temp_Value;
      end if;
   end Mem_Read_Floating_Bus;

   --------------------
   -- Mem_Set_Paging --
   --------------------

   procedure Mem_Set_Paging
     (C          : in out Computer; Mem : not null access RAM_All_Banks;
      Address    :        Unsigned_16; Write_Value : Unsigned_8;
      Read_Value :    out Unsigned_8; Cycles_Left : Natural)
   is
      Offset        : constant Unsigned_8 := Unsigned_8 (Address and 16#FF#);
      Last_Mem_Mode : constant Mem_Mode_Flags := C.Mem_Mode;
   begin
      if Offset >= 16#80# and Offset <= 16#8F# then
         declare
            Write_RAM : constant Boolean := (Offset and 1) /= 0;
         begin
            C.Mem_Mode :=
              C.Mem_Mode and
              not
              (Flag_High_RAM_Bank_2 or Flag_High_RAM or Flag_High_RAM_Write);

            if Write_RAM then
               C.Mem_Mode := C.Mem_Mode or Flag_High_RAM_Write;
            end if;

            if (Offset and 8) = 0 then
               C.Mem_Mode := C.Mem_Mode or Flag_High_RAM_Bank_2;
            end if;

            if Shift_Right (Offset and 2, 1) = (Offset and 1) then
               C.Mem_Mode := C.Mem_Mode or Flag_High_RAM;
            end if;
         end;
      elsif not Is_Apple2 (C) then
         case Offset is
            when 16#00# =>
               C.Mem_Mode := C.Mem_Mode and not Flag_80_Store;
            when 16#01# =>
               C.Mem_Mode := C.Mem_Mode or Flag_80_Store;
            when 16#02# =>
               C.Mem_Mode := C.Mem_Mode and not Flag_Aux_Read;
            when 16#03# =>
               C.Mem_Mode := C.Mem_Mode or Flag_Aux_Read;
            when 16#04# =>
               C.Mem_Mode := C.Mem_Mode and not Flag_Aux_Write;
            when 16#05# =>
               C.Mem_Mode := C.Mem_Mode or Flag_Aux_Write;
            when 16#06# =>
               C.Mem_Mode := C.Mem_Mode and not Flag_Slot_CX_ROM;
            when 16#07# =>
               C.Mem_Mode := C.Mem_Mode or Flag_Slot_CX_ROM;
            when 16#08# =>
               C.Mem_Mode := C.Mem_Mode and not Flag_Alt_ZP;
            when 16#09# =>
               C.Mem_Mode := C.Mem_Mode or Flag_Alt_ZP;
            when 16#0A# =>
               C.Mem_Mode := C.Mem_Mode and not Flag_Slot_C3_ROM;
            when 16#0B# =>
               C.Mem_Mode := C.Mem_Mode or Flag_Slot_C3_ROM;
            when 16#54# =>
               C.Mem_Mode := C.Mem_Mode and not Flag_Page_2;
            when 16#55# =>
               C.Mem_Mode := C.Mem_Mode or Flag_Page_2;
            when 16#56# =>
               C.Mem_Mode := C.Mem_Mode and not Flag_Hi_Res;
            when 16#57# =>
               C.Mem_Mode := C.Mem_Mode or Flag_Hi_Res;
            when 16#71# | 16#73# =>
               --  16#71# - extended memory aux page number
               --  16#73# - RAMWorks III set aux page number
               if Write_Value < RAM_Works_Banks then
                  declare
                     New_Bank : constant RAM_Bank_Index :=
                       RAM_Bank_Index (Write_Value) + RAM_Bank_Aux;
                  begin
                     if New_Bank /= C.RAM_Active_Bank then
                        C.RAM_Active_Bank := New_Bank;

                        C.RAM_Max_Bank_Used :=
                          RAM_Bank_Index'Max (C.RAM_Max_Bank_Used, New_Bank);

                        Mem_Update_Paging (C);
                     end if;
                  end;
               end if;
            when others =>
               null;
         end case;
      end if;

      --  If the memory paging mode has changed, update our memory images
      --  and write tables.

      if Last_Mem_Mode /= C.Mem_Mode then
         Mem_Update_Paging (C);
      end if;

      if Offset <= 1 or (Offset >= 16#54# and Offset <= 16#57#) then
         Video_Set_Mode (Apple2_Base (C), Address);
      end if;

      Mem_Read_Floating_Bus (C, Mem, Read_Value, Cycles_Left);
   end Mem_Set_Paging;

   ---------------
   -- Mem_Reset --
   ---------------

   procedure Mem_Reset
     (C : in out Computer; Mem : not null access RAM_All_Banks)
   is
   begin
      --  Initialize the paging tables

      --  Note: reads from $C000..$CFFF always get special handling
      C.Mem_Read_Bank := (others => RAM_Bank_Main);

      --  Note: writes to $C000..$CFFF always get special handling
      C.Mem_Write_Bank := (others => RAM_Bank_Main);

      --  Initialize the first two RAM banks
      if C.Mem_Init_Pattern = Pattern_FF_FF_00_00 then
         declare
            I : Natural := 0;
         begin
            --  Pattern fill first 64K of RAM
            while I <= 16#FFFF# loop
               Mem (I .. I + 3) := (16#FF#, 16#FF#, 16#00#, 16#00#);

               I := I + 4;
            end loop;

            --  Pattern fill second 64K of RAM (ignoring the RAMWorks banks)
            I := 16#03_0000#;
            while I <= 16#03_FFFF# loop
               Mem (I .. I + 3) := (16#FF#, 16#FF#, 16#00#, 16#00#);

               I := I + 4;
            end loop;
         end;
      else
         Mem (16#00_0000# .. 16#00_FFFF#) := (others => 0);
         Mem (16#03_0000# .. 16#03_FFFF#) := (others => 0);
      end if;

      --  Initialize paging, filling in the 64k memory image
      Mem_Reset_Paging (C);

      --  Initialize & reset the cpu
      --    Do this after ROM has been copied back to mem, so that PC is
      --      correctly init'ed from 6502's reset vector
      CPU_Initialize (C);
   end Mem_Reset;

end Apple2.Memory;
