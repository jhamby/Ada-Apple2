--  Ada-Apple2 : An Apple //e emulator in Ada
--
--  Copyright (C) 2023, Jake Hamby
--
--  Ada-Apple2 is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Ada-Apple2 is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Ada-Apple2; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;

package body Emu.Config_File with
  SPARK_Mode => Off
is

   function Get_Config_File_Name (App_Name : String) return String is
      Config_File_Base : Unbounded_String;
      Config_File_Name : Unbounded_String;
      Home_Dir         : String_Access;
      Conf_File_Name   : constant String := App_Name & ".conf";
   begin
      --  Look for config file relative to user home directory.
      --  If $HOME isn't defined, or "~/.config" doesn't exist, we'll
      --  use the current directory. If "~/.config/linapple" doesn't
      --  exist, we'll try to create the directory inside of "~/.config".

      Home_Dir := String_Access (GNAT.OS_Lib.Getenv ("HOME"));

      if Home_Dir /= null and then Home_Dir'Length /= 0 then
         Set_Unbounded_String (Config_File_Base, Home_Dir.all);
      end if;

      --  GNAT requires us to deallocate the string from Getenv
      Free (Home_Dir);

      --  Does the .config directory exist?

      Set_Unbounded_String
        (Config_File_Name,
         Ada.Directories.Compose (To_String (Config_File_Base), ".config"));

      if Ada.Directories.Exists (To_String (Config_File_Name)) then
         Config_File_Base := Config_File_Name;

         --  Does the linapple directory exist?

         Set_Unbounded_String
           (Config_File_Name,
            Ada.Directories.Compose (To_String (Config_File_Base), App_Name));

         --  If not, try to create App_Name inside ".config"

         if not Ada.Directories.Exists (To_String (Config_File_Name)) then
            Ada.Directories.Create_Directory (To_String (Config_File_Name));
         end if;

         Config_File_Base := Config_File_Name;

      end if;

      --  If there's no "~/.config", then use the current directory

      Set_Unbounded_String
        (Config_File_Name,
         Ada.Directories.Compose
           (To_String (Config_File_Base), Conf_File_Name));

      return To_String (Config_File_Name);

   exception
      when others =>
         Ada.Text_IO.Put_Line ("exception searching for config dir");
         return Conf_File_Name;
   end Get_Config_File_Name;

end Emu.Config_File;
