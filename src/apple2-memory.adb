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

   procedure IO_Access_Cxxx
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; Value : in out Unsigned_8; Is_Write : Boolean);
   --  Read or write a byte from I/O, soft switches, or ROMs ($C000 .. $CFFF)

   procedure IO_Write_C00x (C : in out Computer; Address : Unsigned_16);
   --  Write a byte to a mode switch in $C000 .. $C00F

   procedure IO_Read_C01x
     (C : in out Computer; Address : Unsigned_16; Value : out Unsigned_8);
   --  Read from keyboard or a mode switch in $C010 .. $C01F

   procedure IO_Access_C05x
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; Value : in out Unsigned_8; Is_Write : Boolean);
   --  Read or write a byte from soft switches in $C050 .. $C05F

   procedure IO_Read_C06x
     (C       : in out Computer; Mem : not null access constant RAM_All_Banks;
      Address :        Unsigned_16; Value : out Unsigned_8);
   --  Read joystick/paddle buttons and axes in $C060 .. $C06F

   procedure IO_Access_C07x
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; Value : in out Unsigned_8; Is_Write : Boolean);
   --  Read or write a byte from soft switches in $C070 .. $C07F

   procedure IO_Read_C08x (C : in out Computer; Address : Unsigned_16);
   --  Update bank select switches in $C080 .. $C08F

   procedure IO_Read_C1xx
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Value : in out Unsigned_8);
   --  Read ROMs or floating bus

   procedure IO_Annunciator (ID : Unsigned_8; Enable : Boolean);
   --  Print debug line if program uses the annunciators

   -------------------
   -- Mem_IO_Access --
   -------------------

   overriding procedure Mem_IO_Access
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; Value : in out Unsigned_8; Is_Write : Boolean)
   is
      Mode         : constant Mode_Flags := C.Mode;  --  local read-only copy
      Bank : RAM_Bank_Index      := RAM_Bank_Main;  --  bank 0 unless changed
      Real_Address : Unsigned_16 := Address;  --  modifiable copy of Address
   begin
      if (Address and 16#FE00#) = 0 then
         --  zero and stack page bank select rules ($0000 .. $01FF)
         if Mode.Mem_Alt_ZP then
            Bank := C.RAM_Active_Aux_Bank;
         end if;

      elsif (Address and 16#C000#) /= 16#C000# then
         --  Main 48K RAM bank select rules ($0200 .. $BFFF)
         declare
            Page : constant Unsigned_8 :=
              Unsigned_8 (Shift_Right (Address, 8));
         begin
            --  aux. display page control switches take precedence over
            --  aux. RAM switches for the pages being used for the display
            if Mode.Mem_80_Store
              and then
              ((Page >= 16#04# and Page < 16#08#)
               or else
               (Mode.Video_Hi_Res and then (Page >= 16#20# and Page < 16#40#)))
            then
               if Mode.Video_Page_2 then
                  Bank := C.RAM_Active_Aux_Bank;
               end if;
            else
               if Is_Write then
                  if Mode.Mem_Aux_Write then
                     Bank := C.RAM_Active_Aux_Bank;
                  end if;
               else
                  if Mode.Mem_Aux_Read then
                     Bank := C.RAM_Active_Aux_Bank;
                  end if;
               end if;
            end if;
         end;

      elsif (Address and 16#3000#) /= 0 then
         --  ROM and high RAM bank select rules ($D000 .. $FFFF)
         if Is_Write then

            --  Handle the write case first
            if not Mode.Mem_High_RAM_Write then
               --  current mode discards writes to bank-switched RAM.
               --  Return here instead of falling through to Mem_Access.
               return;
            else
               if Mode.Mem_Alt_ZP then
                  Bank := C.RAM_Active_Aux_Bank;
               end if;
            end if;

            --  The RAM at $Cxxx can be mapped to $Dxxx using this switch
            if (Address and 16#3000#) = 1 and not Mode.Mem_High_RAM_Bank_2 then
               Real_Address := Address - 16#1000#;
            end if;

         else
            --  Reading is similar to writing, but with ROMs added
            if not Mode.Mem_High_RAM then
               Value := C.System_ROM (Natural (Address - 16#D000#));
               return;  --  don't fall through to Mem_Access
            else
               if Mode.Mem_Alt_ZP then
                  Bank := C.RAM_Active_Aux_Bank;
               end if;

               --  The RAM at $Cxxx can be mapped to $Dxxx using this switch
               if (Address and 16#3000#) = 1 and not Mode.Mem_High_RAM_Bank_2
               then
                  Real_Address := Address - 16#1000#;
               end if;
            end if;
         end if;

      else
         --  4K of I/O, soft switches, and ROMs ($C000 .. $CFFF)
         --  Handle separately and then return (don't touch $Cxxx RAM)
         IO_Access_Cxxx (C, Mem, Address, Value, Is_Write);
         return;

      end if;

      --  Read or write the value to the computed memory bank and address.
      --  All code paths except write-disabled writes and $Cxxx arrive here.
      Mem_Access (Mem, Bank, Real_Address, Value, Is_Write);

   end Mem_IO_Access;

   --------------------
   -- IO_Access_Cxxx --
   --------------------

   procedure IO_Access_Cxxx
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; Value : in out Unsigned_8; Is_Write : Boolean)
   is
      Middle_Byte : constant Unsigned_8 :=
        Unsigned_8 (Shift_Right (Address, 4) and 16#FF#);
   begin
      --  Use the middle two nybbles to decide what to do next
      case Middle_Byte is
         when 16#00# =>
            --  Keyboard read and mode switches
            if Is_Write then
               if not Is_Apple2 (C) then
                  IO_Write_C00x (C, Address);
               end if;
            else
               Keyb_Read_Data (Apple2_Base (C), Value);
            end if;

         when 16#01# =>
            --  Keyboard strobe reset, any key down, and mode switches
            if Is_Write then
               Keyb_Reset_Flag (Apple2_Base (C));
            else
               IO_Read_C01x (C, Address, Value);
            end if;

         when 16#02# | 16#04# | 16#0B# | 16#0F# =>
            --  Cassette interface, unused, empty slots 3 and 7
            if not Is_Write then
               Mem_Read_Floating_Bus (C, Mem, Value);
            end if;

         when 16#03# =>
            --  Speaker click (writes may not be audible if CPU writes twice)
            Spkr_Toggle (Apple2_Base (C), Is_Write);

         when 16#05# =>
            --  Video / Memory
            IO_Access_C05x (C, Mem, Address, Value, Is_Write);

         when 16#06# =>
            --  Joystick
            if not Is_Write then
               IO_Read_C06x (C, Mem, Address, Value);
            end if;

         when 16#07# =>
            --  Joystick timer reset / Video / RAMWorks bank
            IO_Access_C07x (C, Mem, Address, Value, Is_Write);

         when 16#08# =>
            --  memory bank select switches
            if not Is_Write then
               IO_Read_C08x (C, Address);
               Mem_Read_Floating_Bus (C, Mem, Value);
            else
               Put_Line ("IO_Access_Cxxx - ignoring write to $C08x");
            end if;

         when 16#09# =>
            --  slot 1 (parallel printer card)
            Print_Status (Apple2_Base (C), Value);

         when 16#0A# =>
            --  slot 2 (super serial card)
            SSC_Access (Apple2_Base (C), Address, Value, Is_Write);

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
            Disk_IO_Access (Apple2_Base (C), Address, Value, Is_Write);

         when 16#40# .. 16#5F# =>
            --  slots 4 and 5 $C400 .. $C5FF space (Mockingboard / Phasor)
            MB_Access (Apple2_Base (C), Address, Value, Is_Write);

         when others =>
            --  read ROM or floating bus
            if not Is_Write then
               IO_Read_C1xx (C, Mem, Address, Value);
            end if;

      end case;

   end IO_Access_Cxxx;

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
      Address :    Unsigned_16; Value : in out Unsigned_8; Is_Write : Boolean)
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

      if not Is_Write then
         Mem_Read_Floating_Bus (C, Mem, Value);
      end if;

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

   --------------------
   -- IO_Access_C07x --
   --------------------

   procedure IO_Access_C07x
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; Value : in out Unsigned_8; Is_Write : Boolean)
   is
      Offset : constant Unsigned_4 := Unsigned_4 (Address and 16#0F#);
   begin
      --  All accesses reset the joystick resistance timer
      Joy_Reset_Position (Apple2_Base (C));

      case Offset is
         when 16#01# | 16#03# =>
            if Is_Write then
               --  16#71# - extended memory aux page number
               --  16#73# - RAMWorks III set aux page number
               if Value < RAM_Works_Banks then
                  declare
                     New_Bank : constant RAM_Bank_Index :=
                       RAM_Bank_Index (Value) + RAM_Bank_Aux_Start;
                  begin
                     if New_Bank /= C.RAM_Active_Aux_Bank then
                        C.RAM_Active_Aux_Bank := New_Bank;

                        C.RAM_Max_Bank_Used :=
                          RAM_Bank_Index'Max (C.RAM_Max_Bank_Used, New_Bank);
                     end if;
                  end;
               end if;
            else
               Mem_Read_Floating_Bus (C, Mem, Value);
            end if;

         when 16#0F# =>
            --  read DHIRES mode (Apple IIe)
            if not Is_Write then
               if C.Mode.Video_Dbl_Hi_Res_Visible then
                  Mem_Read_Floating_Bus
                    (C, Mem, C.Mode.Video_Dbl_Hi_Res, Value);
               else
                  Mem_Read_Floating_Bus (C, Mem, Value);
               end if;
            end if;

         when others =>
            if not Is_Write then
               Mem_Read_Floating_Bus (C, Mem, Value);
            end if;

      end case;
   end IO_Access_C07x;

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
     (C       : in out Computer; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Value : in out Unsigned_8)
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
                (Natural (Address - Expansion_ROM_Start));
            return;
         end if;
      elsif C.Mode.Mem_Slot_CX_ROM or (C.Mode.Mem_Slot_C3_ROM and Slot = 3)
      then
         Value := C.Card_ROMs (Slot) (Natural (Address and 16#FF#));
         return;
      end if;

      --  If we reach this point, return either built-in ROM or floating bus
      if Is_Apple2 (C) then
         Mem_Read_Floating_Bus (C, Mem, Value);
      else
         Value := C.Extra_ROM (Natural (Address - Apple_IO_Start));
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
            C.System_ROM := Apple_2_ROM;
            --  12KB ROM

         when Apple_2_Plus =>
            C.System_ROM := Apple_2_Plus_ROM;
            --  12KB ROM

         when Apple_2e =>
            C.System_ROM :=
              Apple_2e_ROM (Extra_ROM_Size .. Apple_2e_ROM_Size - 1);
            C.Extra_ROM  := Apple_2e_ROM (0 .. Extra_ROM_Size - 1);
            --  16KB ROM

         when others =>
            C.System_ROM :=
              Apple_2e_Enhanced_ROM (Extra_ROM_Size .. Apple_2e_ROM_Size - 1);
            C.Extra_ROM  := Apple_2e_Enhanced_ROM (0 .. Extra_ROM_Size - 1);
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

   ---------------------------
   -- Mem_Read_Floating_Bus --
   ---------------------------

   procedure Mem_Read_Floating_Bus
     (C     : in out Computer; Mem : not null access constant RAM_All_Banks;
      Value : in out Unsigned_8)
   is
      Address : Unsigned_16;
   begin
      Address := Video_Get_Scanner_Address (Apple2_Base (C));
      Mem_Read (Mem, RAM_Bank_Main, Address, Value);
   end Mem_Read_Floating_Bus;

   --------------------------------------
   -- Mem_Read_Floating_Bus (high bit) --
   --------------------------------------

   procedure Mem_Read_Floating_Bus
     (C        : in out Computer; Mem : not null access constant RAM_All_Banks;
      High_Bit :        Boolean; Value : in out Unsigned_8)
   is
      Temp_Value : Unsigned_8;
   begin
      Mem_Read_Floating_Bus (C, Mem, Temp_Value);
      Temp_Value := Temp_Value and 16#7F#;
      if High_Bit then
         Value := Temp_Value or 16#80#;
      else
         Value := Temp_Value;
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

      --  DHIRES replaces annunciator outputs on Apple IIe unless disabled
      if not Is_Apple2 (C) then
         C.Mode.Video_Dbl_Hi_Res_Visible := True;
      end if;

      C.RAM_Max_Bank_Used := RAM_Bank_Aux_Start;

      --  Initialize the first two RAM banks with 16#FF00# pattern
      declare
         I : Natural := 0;
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
