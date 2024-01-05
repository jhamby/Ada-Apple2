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

with Ada.Text_IO;

with Emu.ROM_File; use Emu.ROM_File;

package body Apple2 with
  SPARK_Mode
is
   Res_Path : constant String := "res";

   ---------------
   -- Is_Apple2 --
   ---------------

   function Is_Apple2 (C : Apple2_Base) return Boolean is
   begin
      return C.Settings.Model = Apple_2 or C.Settings.Model = Apple_2_Plus;
   end Is_Apple2;

   -----------------------
   -- Get_Apple_2_Model --
   -----------------------

   function Get_Apple_2_Model (C : Apple2_Base) return Apple_2_Model is
   begin
      return C.Settings.Model;
   end Get_Apple_2_Model;

   -----------------------
   -- Cycles_Since_Boot --
   -----------------------

   function Cycles_Since_Boot (C : Apple2_Base) return CPU_Cycle_Count is
   begin
      if C.Settings.Video_Standard = NTSC then
         return
           (C.Frames_Since_Boot * NTSC_Clocks_Per_Frame) +
           CPU_Cycle_Count ((C.Scan_Line * Cycles_Per_Line) + C.Column_Cycle);
      else
         return
           (C.Frames_Since_Boot * PAL_Clocks_Per_Frame) +
           CPU_Cycle_Count ((C.Scan_Line * Cycles_Per_Line) + C.Column_Cycle);
      end if;
   end Cycles_Since_Boot;

   ---------------
   -- Load_ROMs --
   ---------------

   procedure Load_ROMs
     (C : in out Apple2_Base; Status : out File_Op_Status_Type)
   is
      Size : Unsigned_32;
   begin
      ROM_File_Load
        (C.Keyboard_ROM, Res_Path,
         "Apple_IIe_Keyboard_ROM_341-0132-D_US-Dvorak_1984.bin", Status, Size);

      if Status /= File_Success then
         return;
      end if;

      if Size /= Keyboard_ROM_Size then
         Ada.Text_IO.Put_Line ("unexpected keyboard ROM size");
         Status := File_Error;
         return;
      end if;

      case C.Settings.Model is
         when Apple_2 | Apple_2_Plus =>
            ROM_File_Load
              (C.Video_ROM, Res_Path,
               "Apple_II_plus_Video_ROM_341-0036_Rev_7.bin", Status, Size);

         when Apple_2e =>
            ROM_File_Load
              (C.Video_ROM, Res_Path,
               "Apple_IIe_Video_ROM_342-0133-A_US_1982.bin", Status, Size);

         when others =>
            ROM_File_Load
              (C.Video_ROM, Res_Path,
               "Apple_IIe_Enhanced_Video_ROM_342-0265-A_US_1983.bin", Status,
               Size);

      end case;

      C.Video_ROM_Size := Size;
   end Load_ROMs;

end Apple2;
