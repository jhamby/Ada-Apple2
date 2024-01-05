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

package Emu.ROM_File with
  SPARK_Mode
is

   procedure ROM_File_Load
     (ROM    : in out Mem_Byte_Range; File_Path, File_Name : String;
      Status :    out File_Op_Status_Type; Size : out Unsigned_32);
   --  Load binary image from the specified file and return status

end Emu.ROM_File;
