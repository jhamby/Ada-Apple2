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
with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;

with System;

package body Emu.ROM_File with
  SPARK_Mode => Off
is
   -------------------
   -- ROM_File_Load --
   -------------------

   procedure ROM_File_Load
     (ROM    : in out Mem_Byte_Range; File_Path, File_Name : String;
      Status :    out File_Op_Status_Type; Size : out Unsigned_32)
   is
      type SEA_Pointer is
        access all Stream_Element_Array
          (Stream_Element_Offset (ROM'First) ..
               Stream_Element_Offset (ROM'Last));

      function As_SEA_Pointer is new Ada.Unchecked_Conversion
        (System.Address, SEA_Pointer);
   begin
      if not Ada.Directories.Exists (File_Path) then
         Ada.Text_IO.Put_Line ("ROM directory '" & File_Path & "' not found.");
         Status := Dir_Not_Found;
         Size   := 0;
         return;
      end if;

      declare
         ROM_File_Name : constant String :=
           Ada.Directories.Compose (File_Path, File_Name);

         ROM_File   : File_Type;
         ROM_Stream : Stream_Access;
         ROM_Offset : Stream_Element_Offset;
      begin
         if not Ada.Directories.Exists (ROM_File_Name) then
            Ada.Text_IO.Put_Line
              ("ROM file '" & ROM_File_Name & "' not found.");
            Status := File_Not_Found;
            Size   := 0;
            return;
         end if;

         Open (ROM_File, In_File, ROM_File_Name);
         ROM_Stream := Stream (ROM_File);

         Ada.Streams.Read
           (ROM_Stream.all, As_SEA_Pointer (ROM'Address).all, ROM_Offset);

         Close (ROM_File);
         Status := File_Success;
         Size   := Unsigned_32 (ROM_Offset + 1);

         Ada.Text_IO.Put_Line
           ("ROM file '" & ROM_File_Name & "' size: " & Size'Image);
      end;

   exception
      when others =>
         Ada.Text_IO.Put_Line ("exception while loading ROM");
         Status := File_Error;
         Size   := 0;
   end ROM_File_Load;

end Emu.ROM_File;
