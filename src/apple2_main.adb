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

--  Description: main
--  Author: Various

--  Adaptation for SDL and POSIX (l) by beom beotiger, Nov-Dec 2007,
--    krez beotiger March 2012 AD

--  Linappple-pie was adapted in OCT 2015 for use with Retropie.
--  By Mark Ormond.

with Ada.Text_IO;

with Apple2; use Apple2;

with Apple2.Memory;   use Apple2.Memory;
with Apple2.Settings; use Apple2.Settings;

with Emu; use Emu;

with Emu.Config_File; use Emu.Config_File;
with Emu.Memory;      use Emu.Memory;
with Emu.Registry;    use Emu.Registry;

with Interfaces; use Interfaces;

with SDL.Video.Palettes; use SDL.Video.Palettes;

procedure Apple2_Main is

   Config_File_Name : constant String := Get_Config_File_Name ("linapple");
   --  Get the config file pathname before we begin

   Registry : Registry_Type;

   File_Op_Status : File_Op_Status_Type;  --  TODO: show I/O errors in GUI

   ---------------------------------------
   --  TODO: remove old test code below --
   ---------------------------------------

   procedure Run_Tests (C : in out Computer; Mem : access RAM_All_Banks);
   --  Exercise the computer object

   procedure Run_Tests (C : in out Computer; Mem : access RAM_All_Banks) is
      IO_Read_Value : Unsigned_8;
   begin
      for I in Unsigned_16 (16#C000#) .. Unsigned_16 (16#CFFF#) loop
         Mem_IO_Read (C, Mem, I, IO_Read_Value, 3);
         Mem_IO_Write (C, Mem, I, 123, 3);
      end loop;

      CPU_Execute (C, Mem, 10_000);

      if IO_Read_Value /= 0 then
         Ada.Text_IO.Put_Line ("I/O read value = " & IO_Read_Value'Image);
      end if;
   end Run_Tests;

   C : Computer;
   --  Test computer object

begin

   Ada.Text_IO.Put_Line ("Config file name: " & Config_File_Name);

   Initialize (Registry, Config_File_Name);

   Init_Registry (C.Settings, Registry);  --  init registry from settings

   Load_Settings (Registry, File_Op_Status);

   if File_Op_Status = File_Error then
      Ada.Text_IO.Put_Line ("error reading config file");
      --  Perhaps the GUI can show file I/O errors in the future, but we're
      --  going to continue running with default settings, regardless.
   end if;

   --  TODO: remove old test code below
   declare
      type Global_RAM_Access is access RAM_All_Banks;
      --  The emulated RAM will be freed when this type goes out of scope

      Main_Mem : constant Global_RAM_Access := new RAM_All_Banks;
      --  Note: the access variable is constant, but the RAM itself is not
   begin
      Init_Settings (C.Settings, Registry);

      Init_Apple2 (C, Main_Mem);
      Run_Tests (C, Main_Mem);

      Set_Video_Standard (C.Settings, Registry, PAL);
      Set_Video_Standard (C.Settings, Registry, NTSC);
      Set_Monochrome_Color
        (C.Settings, Registry, RGB_Colour'(16#CC#, 16#CC#, 16#CC#));
   end;

   End_Registry_Write_Task (Registry);

end Apple2_Main;
