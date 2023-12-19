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

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Apple2; use Apple2;

with Apple2.Memory;   use Apple2.Memory;
with Apple2.Settings; use Apple2.Settings;

with Emu; use Emu;

with Emu.Config_File; use Emu.Config_File;
with Emu.Memory;      use Emu.Memory;
with Emu.Registry;    use Emu.Registry;

with GNAT.OS_Lib;

procedure Apple2_Main is

   Config_File_Name : String := Get_Config_File_Name ("linapple");
   --  Get the config file pathname for this app (the user can override)

   procedure Print_Help;

   procedure Parse_Command_Line (S : in out Settings_Type);
   --  Update settings with command-line options before copying to registry

   Benchmark_Mode : Boolean := False;

   --  Helper procedures

   procedure Print_Help is
   begin
      Put_Line ("usage: " & Command_Name & " [options]");
      New_Line;
      Put_Line
        ("Ada-Apple2 is an emulator for Apple ][, Apple ][+, " &
         "Apple //e, and Enhanced Apple //e computers.");
      New_Line;
      Put_Line ("  -h|--help      show this help message");
      Put_Line
        ("  --conf <file>  use <file> instead of any default " &
         "config files");
      Put_Line ("  --d1 <file>      insert disk image into first drive");
      Put_Line ("  --d2 <file>      insert disk image into second drive");
      Put_Line ("  -b|--autoboot    boot/reset at startup");
      Put_Line ("  -f|--fullscreen  run fullscreen");
      Put_Line ("  --benchmark      benchmark and quit");
      New_Line;
   end Print_Help;

   procedure Parse_Command_Line (S : in out Settings_Type) is
      Skip_Next : Boolean := False;
   begin
      for I in 1 .. Argument_Count loop
         declare
            Arg : constant String := Argument (I);
         begin
            if Skip_Next then
               Skip_Next := False;

            elsif Arg = "-b" or else Arg = "--autoboot" then
               S.Boot_At_Startup := True;

            elsif Arg = "-f" or else Arg = "--fullscreen" then
               S.Fullscreen := True;

            elsif Arg = "--benchmark" then
               Benchmark_Mode := True;

            elsif Arg = "--conf" and then I < Argument_Count then
               Config_File_Name := Argument (I + 1);
               Skip_Next        := True;

            elsif Arg = "--d1" and then I < Argument_Count then
               Set_Unbounded_String
                 (S.Disk_Image_Filenames (0), Argument (I + 1));
               Skip_Next := True;

            elsif Arg = "--d2" and then I < Argument_Count then
               Set_Unbounded_String
                 (S.Disk_Image_Filenames (1), Argument (I + 1));
               Skip_Next := True;

            else
               Print_Help;
               GNAT.OS_Lib.OS_Exit (2);
            end if;
         end;
      end loop;
   end Parse_Command_Line;

   C : Computer;  --  Test computer object

   Registry : Registry_Type;  --  Pass to any code that saves user settings

   File_Op_Status : File_Op_Status_Type;  --  TODO: show I/O errors in GUI

begin

   Parse_Command_Line (C.Settings);  --  call first to get config file name

   Initialize (Registry, Config_File_Name);  --  set filename and start task

   Init_Registry (C.Settings, Registry);  --  init registry from settings

   Load_Settings (Registry, File_Op_Status);  --  load config file

   Init_Settings (C.Settings, Registry);  --  update settings from registry

   Parse_Command_Line (C.Settings);  --  call again to override config file

   if File_Op_Status = File_Error then
      Ada.Text_IO.Put_Line ("error reading config file");
      --  Perhaps the GUI can show file I/O errors in the future, but we're
      --  going to continue running with default settings, regardless.
   end if;

   declare
      type Global_RAM_Access is access RAM_All_Banks;
      --  The emulated RAM will be freed when this type goes out of scope

      Main_Mem : constant Global_RAM_Access := new RAM_All_Banks;
      --  Note: the access variable is constant, but the RAM itself is not
   begin
      Init_Apple2 (C, Main_Mem);

      if Benchmark_Mode then
         Put_Line ("Benchmark mode will go here!");
      else
         Put_Line ("SDL initializing.");
      end if;
   end;

   End_Registry_Write_Task (Registry);

end Apple2_Main;
