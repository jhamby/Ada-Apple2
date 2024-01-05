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

with Ada.Text_IO; use Ada.Text_IO;

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

   procedure IO_Write_C00x (C : in out Computer; Address : Unsigned_16) with
     Inline;
   --  Write a byte to a mode switch in $C000 .. $C00F

   procedure IO_Read_C01x
     (C : in out Computer; Address : Unsigned_16; Value : out Unsigned_8) with
     Inline;
   --  Read from keyboard or a mode switch in $C010 .. $C01F

   procedure IO_Access_C05x
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16);
   --  Read or write a byte from soft switches in $C050 .. $C05F

   procedure IO_Read_C06x
     (C       : in out Computer; Mem : not null access constant RAM_All_Banks;
      Address :        Unsigned_16; Value : out Unsigned_8) with
     Inline;
   --  Read joystick/paddle buttons and axes in $C060 .. $C06F

   procedure IO_Read_C07x
     (C       : in out Computer; Mem : not null access constant RAM_All_Banks;
      Address :        Unsigned_16; Value : out Unsigned_8) with
     Inline;
   --  Read a byte from soft switches in $C070 .. $C07F

   procedure IO_Write_C07x
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Value : Unsigned_8) with
     Inline;
   --  Write a byte to soft switches in $C070 .. $C07F

   procedure IO_Read_C08x (C : in out Computer; Address : Unsigned_16) with
     Inline;
   --  Update bank select switches in $C080 .. $C08F

   procedure IO_Read_C1xx
     (C       : in out Computer; Mem : not null access constant RAM_All_Banks;
      Address :        Unsigned_16; Value : out Unsigned_8) with
     Inline;
   --  Read ROMs or floating bus

   procedure IO_Annunciator (ID : Unsigned_8; Enable : Boolean);
   --  Print debug line if program uses the annunciators

   -------------------------
   -- Mem_IO_Read_Special --
   -------------------------

   overriding procedure Mem_IO_Read_Special
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Value : out Unsigned_8)
   is
      CxNx : constant Unsigned_4 :=
        Unsigned_4 (Shift_Right (Address, 4) and 16#0F#);
   begin
      if (Address and 16#CF00#) = 16#C000# then
         case CxNx is
            when 16#00# =>
               --  Keyboard read and mode switches
               Keyb_Read_Data (Apple2_Base (C), Value);

            when 16#01# =>
               --  Keyboard strobe reset, any key down, and mode switches
               IO_Read_C01x (C, Address, Value);

            when 16#02# | 16#04# | 16#0B# | 16#0F# =>
               --  Cassette interface, unused, empty slots 3 and 7
               Mem_Read_Floating_Bus (C, Mem, Value);

            when 16#03# =>
               --  Speaker click
               Spkr_Toggle (Apple2_Base (C));

            when 16#05# =>
               --  Video / Memory
               IO_Access_C05x (C, Mem, Address);
               Mem_Read_Floating_Bus (C, Mem, Value);

            when 16#06# =>
               --  Joystick
               IO_Read_C06x (C, Mem, Address, Value);

            when 16#07# =>
               --  Joystick timer reset / Video / RAMWorks bank
               IO_Read_C07x (C, Mem, Address, Value);

            when 16#08# =>
               --  memory bank select switches
               IO_Read_C08x (C, Address);
               Mem_Update_Paging (C, Mem);
               Mem_Read_Floating_Bus (C, Mem, Value);

            when 16#09# =>
               --  slot 1 (parallel printer card)
               Print_Status (Apple2_Base (C), Value);

            when 16#0A# =>
               --  slot 2 (super serial card)
               SSC_Read (Apple2_Base (C), Address, Value);

            when 16#0C# =>
               --  slot 4 (Mockingboard or mouse)
               Phasor_IO (Apple2_Base (C), Mem, Address);
               Mem_Read_Floating_Bus (C, Mem, Value);

            when 16#0D# =>
               --  slot 5 (Phasor sound card)
               Phasor_IO (Apple2_Base (C), Mem, Address);
               Mem_Read_Floating_Bus (C, Mem, Value);

            when 16#0E# =>
               --  slot 6 (Disk ][)
               Disk_IO_Read (Apple2_Base (C), Address, Value);
         end case;
      else
         IO_Read_C1xx (C, Mem, Address, Value);
      end if;
   end Mem_IO_Read_Special;

   --------------------------
   -- Mem_IO_Write_Special --
   --------------------------

   overriding procedure Mem_IO_Write_Special
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Value : Unsigned_8)
   is
      CxNx : constant Unsigned_4 :=
        Unsigned_4 (Shift_Right (Address, 4) and 16#0F#);
   begin
      --  $C400 .. $C5xx are writes to slot 4 and 5 address space
      if (Address and 16#CE00#) = 16#C400# then
         Phasor_IO (Apple2_Base (C), Mem, Address);
      elsif (Address and 16#CF00#) = 16#C000# then
         case CxNx is
            when 16#00# =>
               --  Keyboard read and mode switches
               if not Is_Apple2 (C) then
                  IO_Write_C00x (C, Address);
                  Mem_Update_Paging (C, Mem);
               end if;

            when 16#01# =>
               --  Keyboard strobe reset, any key down, and mode switches
               Keyb_Reset_Flag (Apple2_Base (C));

            when 16#03# =>
               --  Speaker click
               Spkr_Toggle (Apple2_Base (C));

            when 16#05# =>
               --  Video / Memory
               IO_Access_C05x (C, Mem, Address);

            when 16#07# =>
               --  Joystick timer reset / Video / RAMWorks bank
               IO_Write_C07x (C, Mem, Address, Value);

            when 16#09# =>
               --  slot 1 (parallel printer card)
               Print_Transmit (Apple2_Base (C), Value);

            when 16#0A# =>
               --  slot 2 (super serial card)
               SSC_Write (Apple2_Base (C), Address, Value);

            when 16#0C# =>
               --  slot 4 (Mockingboard or mouse)
               Phasor_IO (Apple2_Base (C), Mem, Address);

            when 16#0D# =>
               --  slot 5 (Phasor sound card)
               Phasor_IO (Apple2_Base (C), Mem, Address);

            when 16#0E# =>
               --  slot 6 (Disk ][)
               Disk_IO_Write (Apple2_Base (C), Address, Value);

            when 16#02# | 16#04# | 16#06# | 16#08# | 16#0B# | 16#0F# =>
               --  Cassette interface, unused, empty slots 3 and 7
               null;
         end case;
      end if;
   end Mem_IO_Write_Special;

   -------------------
   -- IO_Write_C00x --
   -------------------

   procedure IO_Write_C00x (C : in out Computer; Address : Unsigned_16) is
      Offset : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      case Offset is
         when 16#00# =>
            C.Mode.Mem_80_Store := False;

         when 16#01# =>
            C.Mode.Mem_80_Store := True;

         when 16#02# =>
            C.Mode.Mem_Aux_Read := False;

         when 16#03# =>
            C.Mode.Mem_Aux_Read := True;

         when 16#04# =>
            C.Mode.Mem_Aux_Write := False;

         when 16#05# =>
            C.Mode.Mem_Aux_Write := True;

         when 16#06# =>
            C.Mode.Mem_Slot_CX_ROM := True;

         when 16#07# =>
            C.Mode.Mem_Slot_CX_ROM := False;

         when 16#08# =>
            C.Mode.Mem_Alt_ZP := False;

         when 16#09# =>
            C.Mode.Mem_Alt_ZP := True;

         when 16#0A# =>
            C.Mode.Mem_Slot_C3_ROM := False;

         when 16#0B# =>
            C.Mode.Mem_Slot_C3_ROM := True;

         when 16#0C# =>
            C.Mode.Video_80_Column := False;

         when 16#0D# =>
            C.Mode.Video_80_Column := True;

         when 16#0E# =>
            C.Mode.Video_Alt_Charset := False;

         when 16#0F# =>
            C.Mode.Video_Alt_Charset := True;
      end case;
   end IO_Write_C00x;

   ------------------
   -- IO_Read_C01x --
   ------------------

   procedure IO_Read_C01x
     (C : in out Computer; Address : Unsigned_16; Value : out Unsigned_8)
   is
      Key_Value : constant Unsigned_8 := Keyb_Get_Keycode (Apple2_Base (C));
      --  Lower 7 bits to return, with switch state in high bit

      Offset   : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
      High_Bit : Boolean;
   begin
      case Offset is
         when 16#00# =>
            Keyb_Read_Flag (Apple2_Base (C), High_Bit);

         when 16#01# =>
            High_Bit := C.Mode.Mem_High_RAM_Bank_2;

         when 16#02# =>
            High_Bit := C.Mode.Mem_High_RAM;

         when 16#03# =>
            High_Bit := C.Mode.Mem_Aux_Read;

         when 16#04# =>
            High_Bit := C.Mode.Mem_Aux_Write;

         when 16#05# =>
            High_Bit := not C.Mode.Mem_Slot_CX_ROM;

         when 16#06# =>
            High_Bit := C.Mode.Mem_Alt_ZP;

         when 16#07# =>
            High_Bit := C.Mode.Mem_Slot_C3_ROM;

         when 16#08# =>
            High_Bit := C.Mode.Mem_80_Store;

         when 16#09# =>
            High_Bit := Video_Get_VBL_Bar (Apple2_Base (C));

         when 16#0A# =>
            High_Bit := C.Mode.Video_Text;

         when 16#0B# =>
            High_Bit := C.Mode.Video_Mixed;

         when 16#0C# =>
            High_Bit := C.Mode.Video_Page_2;

         when 16#0D# =>
            High_Bit := C.Mode.Video_Hi_Res;

         when 16#0E# =>
            High_Bit := C.Mode.Video_Alt_Charset;

         when 16#0F# =>
            High_Bit := C.Mode.Video_80_Column;

      end case;

      if High_Bit then
         Value := Key_Value or 16#80#;
      else
         Value := Key_Value;
      end if;
   end IO_Read_C01x;

   --------------------
   -- IO_Access_C05x --
   --------------------

   procedure IO_Access_C05x
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16)
   is
      Offset : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      --  Note: most of these switches work with read or write
      case Offset is
         when 16#00# =>
            C.Mode.Video_Text := False;

         when 16#01# =>
            C.Mode.Video_Text := True;

         when 16#02# =>
            C.Mode.Video_Mixed := False;

         when 16#03# =>
            C.Mode.Video_Mixed := True;

         when 16#04# =>
            C.Mode.Video_Page_2 := False;

         when 16#05# =>
            C.Mode.Video_Page_2 := True;

         when 16#06# =>
            C.Mode.Video_Hi_Res := False;

         when 16#07# =>
            C.Mode.Video_Hi_Res := True;

         when 16#08# .. 16#0D# =>
            if not C.Mode.Video_Dbl_Hi_Res_Visible then
               IO_Annunciator
                 (Shift_Right (Unsigned_8 (Offset) and 16#06#, 1),
                  (Offset and 16#01#) /= 0);
            end if;

         when 16#0E# =>
            if C.Mode.Video_Dbl_Hi_Res_Visible = True then
               C.Mode.Video_Dbl_Hi_Res := True;
            else
               IO_Annunciator (3, False);
            end if;

         when 16#0F# =>
            if C.Mode.Video_Dbl_Hi_Res_Visible = True then
               C.Mode.Video_Dbl_Hi_Res := False;
            else
               IO_Annunciator (3, True);
            end if;

      end case;

      Mem_Update_Paging (C, Mem);

   end IO_Access_C05x;

   ------------------
   -- IO_Read_C06x --
   ------------------

   procedure IO_Read_C06x
     (C       : in out Computer; Mem : not null access constant RAM_All_Banks;
      Address :        Unsigned_16; Value : out Unsigned_8)
   is
      Low_Nybble : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      case Low_Nybble is
         when 16#01# .. 16#03# =>
            Joy_Read_Button
              (Apple2_Base (C), Joystick_Button (Low_Nybble - 1), Value);

         when 16#04# .. 16#07# =>
            Joy_Read_Position
              (Apple2_Base (C), Joystick_Axis (Low_Nybble - 4), Value);

         when others =>
            Mem_Read_Floating_Bus (C, Mem, Value);

      end case;
   end IO_Read_C06x;

   ------------------
   -- IO_Read_C07x --
   ------------------

   procedure IO_Read_C07x
     (C       : in out Computer; Mem : not null access constant RAM_All_Banks;
      Address :        Unsigned_16; Value : out Unsigned_8)
   is
      Offset : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      --  Reads and writes reset the joystick resistance timer
      Joy_Reset_Position (Apple2_Base (C));

      if Offset = 16#0F# and C.Mode.Video_Dbl_Hi_Res_Visible then
         Mem_Read_Floating_Bus (C, Mem, C.Mode.Video_Dbl_Hi_Res, Value);
      else
         Mem_Read_Floating_Bus (C, Mem, Value);
      end if;
   end IO_Read_C07x;

   -------------------
   -- IO_Write_C07x --
   -------------------

   procedure IO_Write_C07x
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Value : Unsigned_8)
   is
      Offset : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      --  Reads and writes reset the joystick resistance timer
      Joy_Reset_Position (Apple2_Base (C));

      if (Offset = 16#01# or Offset = 16#03#) and Value < RAM_Works_Banks then
         declare
            New_Bank : constant RAM_Bank_Index :=
              RAM_Bank_Index (Value) + RAM_Bank_Aux_Start;
         begin
            if New_Bank /= C.RAM_Active_Aux_Bank then
               C.RAM_Active_Aux_Bank := New_Bank;

               C.RAM_Max_Bank_Used :=
                 RAM_Bank_Index'Max (C.RAM_Max_Bank_Used, New_Bank);

               Mem_Update_Paging (C, Mem);
            end if;
         end;
      end if;
   end IO_Write_C07x;

   ------------------
   -- IO_Read_C08x --
   ------------------

   procedure IO_Read_C08x (C : in out Computer; Address : Unsigned_16) is
      High_RAM        : constant Boolean :=
        Shift_Right (Address and 2, 1) = (Address and 1);
      High_RAM_Write  : constant Boolean := (Address and 1) /= 0;
      High_RAM_Bank_2 : constant Boolean := (Address and 8) = 0;
   begin
      --  update the bank settings using the bits set in the address
      C.Mode.Mem_High_RAM        := High_RAM;
      C.Mode.Mem_High_RAM_Bank_2 := High_RAM_Bank_2;
      C.Mode.Mem_High_RAM_Write  := False;

      if High_RAM_Write then
         --  This is the only soft switch requiring two reads to activate;
         --  allow up to 32 clock cycles between the two required reads
         if C.Cycles_Since_Boot - C.High_RAM_Write_Latch_Cycle <= 32 then
            --  update the state on the second consecutive read
            C.Mode.Mem_High_RAM_Write := True;
         else
            --  record the cycle count when we saw this read
            C.High_RAM_Write_Latch_Cycle := C.Cycles_Since_Boot;
         end if;
      else
         --  write high RAM bit clear; reset write latch cycle count
         C.High_RAM_Write_Latch_Cycle := 0;
      end if;
   end IO_Read_C08x;

   ------------------
   -- IO_Read_C1xx --
   ------------------

   --  Enabling expansion ROM ($C800..$CFFF]:
   --  . Enable if: Enable1 && Enable2
   --  . Enable1 = I/O SELECT' (6502 accesses $Csxx)
   --    - Reset when 6502 accesses $CFFF
   --  . Enable2 = I/O STROBE' (6502 accesses [$C800..$CFFF])

   procedure IO_Read_C1xx
     (C       : in out Computer; Mem : not null access constant RAM_All_Banks;
      Address :        Unsigned_16; Value : out Unsigned_8)
   is
      IO_Strobe : Boolean                  := False;
      Slot      : constant Full_Slot_Range :=
        Slot_Range (Shift_Right (Address, 8) and 7);
   begin
      if Address = 16#CFFF# then
         --  Disable expansion ROM space ($C100 .. $CFFF)
         C.IO_Select_Slot                := 0;
         C.Mode.Mem_Expansion_ROM_Active := False;
      elsif Address >= Expansion_ROM_Start then
         IO_Strobe := True;
      else
         C.IO_Select_Slot := Slot;
      end if;

      if C.IO_Select_Slot /= 0 and IO_Strobe then
         C.Mode.Mem_Expansion_ROM_Active := True;
      end if;

      if Address >= Expansion_ROM_Start then
         if
           (C.Mode.Mem_Slot_CX_ROM and C.Mode.Mem_Expansion_ROM_Active and
            C.IO_Select_Slot /= 0)
           and then C.Expansion_ROMs (C.IO_Select_Slot) /= null
         then
            Value :=
              C.Expansion_ROMs (C.IO_Select_Slot)
                (Unsigned_32 (Address - Expansion_ROM_Start));
            return;
         end if;
      elsif C.Mode.Mem_Slot_CX_ROM or (C.Mode.Mem_Slot_C3_ROM and Slot = 3)
      then
         Value := C.Card_ROMs (Slot) (Unsigned_32 (Address and 16#FF#));
         return;
      end if;

      --  If we reach this point, return either built-in ROM or floating bus
      if Is_Apple2 (C) then
         Mem_Read_Floating_Bus (C, Mem, Value);
      else
         Value := Mem (16#01_0000# or Unsigned_32 (Address));
         --  Read Apple IIe firmware from ROM bank
      end if;
   end IO_Read_C1xx;

   --------------------
   -- IO_Annunciator --
   --------------------

   procedure IO_Annunciator (ID : Unsigned_8; Enable : Boolean) is
   begin
      Put_Line
        ("Annunciator #" & ID'Image & " is now " &
         (if Enable then "on" else "off"));
   end IO_Annunciator;

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
            --  12KB ROM in second 64K bank

         when Apple_2_Plus =>
            Mem (16#01_D000# .. 16#01_FFFF#) := Apple_2_Plus_ROM;
            --  12KB ROM

         when Apple_2e =>
            Mem (16#01_C000# .. 16#01_FFFF#) := Apple_2e_ROM;
            --  16KB ROM in second 64KB bank

         when others =>
            Mem (16#01_C000# .. 16#01_FFFF#) := Apple_2e_Enhanced_ROM;
            --  16KB ROM
      end case;

      C.Card_ROMs (1) := Printer_ROM;
      --  $C1xx : Parallel printer ROM

      C.Card_ROMs (2) :=
        SSC_ROM (SSC_Card_Firmware_Offset .. SSC_Card_Firmware_Offset + 255);
      --  $C2xx : SSC slot ROM (256 byte section of 2KB ROM)

      C.Card_ROMs (6) := Disk_ROM;
      --  $C600 : Disk ][ f/w

      C.Expansion_ROMs (2) := SSC_ROM'Access;
      --  $C800 .. $CFFF : SSC 2KB expansion ROM

      Mem_Reset (C, Mem);
   end Init_Apple2;

   -----------------------
   -- Mem_Update_Paging --
   -----------------------

   procedure Mem_Update_Paging
     (C : in out Computer; Mem : not null access RAM_All_Banks)
   is
      Mode       : constant Mode_Flags  := C.Mode;
      Aux_Offset : constant Unsigned_16 :=
        Shift_Left (Unsigned_16 (C.RAM_Active_Aux_Bank), 8);
      ROM_Offset : constant Unsigned_16 :=
        Shift_Left (Unsigned_16 (RAM_Bank_ROMs), 8);
   begin
      --  Zero page and stack page ($0000 .. $01FF)
      if Mode.Mem_Alt_ZP then
         C.Page_Table_Read (16#00#)  := Aux_Offset;
         C.Page_Table_Read (16#01#)  := Aux_Offset or 16#01#;
         C.Page_Table_Write (16#00#) := Aux_Offset;
         C.Page_Table_Write (16#01#) := Aux_Offset or 16#01#;
      else
         C.Page_Table_Read (16#00#)  := 16#0000#;
         C.Page_Table_Read (16#01#)  := 16#0001#;
         C.Page_Table_Write (16#00#) := 16#0000#;
         C.Page_Table_Write (16#01#) := 16#0001#;
      end if;

      --  48K main memory ($0200 .. $BFFF)
      declare
         Read_Page  : constant Unsigned_16 :=
           (if Mode.Mem_Aux_Read then Aux_Offset else 16#0000#);
         Write_Page : constant Unsigned_16 :=
           (if Mode.Mem_Aux_Write then Aux_Offset else 16#0000#);
      begin
         for I in Unsigned_8 (16#02#) .. Unsigned_8 (16#BF#) loop
            C.Page_Table_Read (I)  := Read_Page or Unsigned_16 (I);
            C.Page_Table_Write (I) := Write_Page or Unsigned_16 (I);
         end loop;
      end;

      --  80 column store mapping mode for graphics pages
      if Mode.Mem_80_Store then
         declare
            Video_Page : constant Unsigned_16 :=
              (if Mode.Video_Page_2 then Aux_Offset else 16#0000#);
         begin
            for I in Unsigned_8 (16#04#) .. Unsigned_8 (16#07#) loop
               C.Page_Table_Read (I)  := Video_Page or Unsigned_16 (I);
               C.Page_Table_Write (I) := Video_Page or Unsigned_16 (I);
            end loop;

            if Mode.Video_Hi_Res then
               for I in Unsigned_8 (16#20#) .. Unsigned_8 (16#3F#) loop
                  C.Page_Table_Read (I)  := Video_Page or Unsigned_16 (I);
                  C.Page_Table_Write (I) := Video_Page or Unsigned_16 (I);
               end loop;
            end if;
         end;
      end if;

      --  $Cxxx is always I/O space (memory reads select expansion ROM)
      --  $C000 .. $C0FF - I/O registers for system devices and slots
      --  $C100 .. $CFFF - internal ROM (!SLOTCXROM)
      --  $C300 .. $C3FF - internal ROM (!SLOTCXROM or !SLOTC3ROM)
      --  $Cs00 .. $CsFF - 256-byte slot ROMs or I/O space (SLOTCXROM)
      --  $C800 .. $CFFF - selected expansion ROM or internal ROM

      declare
         --  Alt ZP switch also selects the high RAM bank to use
         Page : constant Unsigned_16 :=
           (if Mode.Mem_High_RAM then
              (if Mode.Mem_Alt_ZP then Aux_Offset else 16#0000#)
            else ROM_Offset);

         --  RAM at $Cxxx can be remapped to $Dxxx with this switch
         --  (two banks of 4K within each 64K RAM bank)
         D_Offset : constant Unsigned_16 :=
           (if Mode.Mem_High_RAM_Bank_2 then 16#00# else 16#10#);
      begin
         --  $D000 .. $DFFF - $Cxxx or $Dxxx, write enable switch, or ROM
         for I in Unsigned_8 (16#D0#) .. Unsigned_8 (16#DF#) loop
            if Mode.Mem_High_RAM_Write then
               C.Page_Table_Write (I) := (Page or Unsigned_16 (I)) - D_Offset;
            else
               C.Page_Table_Write (I) := ROM_Offset;  --  ignored writes
            end if;
            C.Page_Table_Read (I) := (Page or Unsigned_16 (I)) - D_Offset;
         end loop;

         --  $E000 .. $FFFF - write enable switch, plus ROM
         for I in Unsigned_8 (16#E0#) .. Unsigned_8 (16#FF#) loop
            if Mode.Mem_High_RAM_Write then
               C.Page_Table_Write (I) := Page or Unsigned_16 (I);
            else
               C.Page_Table_Write (I) := ROM_Offset;  --  ignored writes
            end if;
            C.Page_Table_Read (I) := Page or Unsigned_16 (I);
         end loop;
      end;

   end Mem_Update_Paging;

   ---------------------------
   -- Mem_Read_Floating_Bus --
   ---------------------------

   procedure Mem_Read_Floating_Bus
     (C     :     Computer; Mem : not null access constant RAM_All_Banks;
      Value : out Unsigned_8)
   is
      Address : Unsigned_16;
   begin
      Address := Video_Get_Scanner_Address (Apple2_Base (C));
      Value   := Mem (Unsigned_32 (Address));
      --  Always read from bank 0 since it's what the CPU might see on the bus
   end Mem_Read_Floating_Bus;

   --------------------------------------
   -- Mem_Read_Floating_Bus (high bit) --
   --------------------------------------

   procedure Mem_Read_Floating_Bus
     (C        : Computer; Mem : not null access constant RAM_All_Banks;
      High_Bit : Boolean; Value : out Unsigned_8)
   is
   begin
      Mem_Read_Floating_Bus (C, Mem, Value);

      if High_Bit then
         Value := Value or 16#80#;
      else
         Value := Value and 16#7F#;
      end if;
   end Mem_Read_Floating_Bus;

   ---------------
   -- Mem_Reset --
   ---------------

   procedure Mem_Reset
     (C : in out Computer; Mem : not null access RAM_All_Banks)
   is
   begin
      --  Reset the mode flags to defaults
      C.Mode :=
        (Mem_High_RAM_Bank_2 => True, Mem_Slot_CX_ROM => True,
         Mem_High_RAM_Write  => True, others => False);

      --  Reset the read and write page tables
      Mem_Update_Paging (C, Mem);

      for I in Unsigned_8 (16#C0#) .. Unsigned_8 (16#CF#) loop
         C.Page_Table_Read (I)  := 16#FFFF#;  --  special handling
         C.Page_Table_Write (I) := 16#FFFF#;
      end loop;

      --  DHIRES replaces annunciator outputs on Apple IIe unless disabled
      if not Is_Apple2 (C) then
         C.Mode.Video_Dbl_Hi_Res_Visible := True;
      end if;

      C.RAM_Max_Bank_Used := RAM_Bank_Aux_Start;

      --  Initialize the first two RAM banks with 16#FF00# pattern
      declare
         I : Unsigned_32 := 0;
      begin
         --  Pattern fill first 64K of RAM
         while I <= 16#FFFF# loop
            Mem (I .. I + 3) := (16#FF#, 16#FF#, 16#00#, 16#00#);

            I := I + 4;
         end loop;

         --  Pattern fill second 64K of RAM (ignoring the RAMWorks banks)
         I := 16#01_0000#;
         while I <= 16#01_FFFF# loop
            Mem (I .. I + 3) := (16#FF#, 16#FF#, 16#00#, 16#00#);

            I := I + 4;
         end loop;
      end;

      --  Initialize & reset the cpu
      --    Do this after ROM has been copied back to mem, so that PC is
      --      correctly init'ed from 6502's reset vector
      CPU_Initialize (C, Mem);
   end Mem_Reset;

end Apple2.Memory;
