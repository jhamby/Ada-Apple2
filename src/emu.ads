--  Core definitions for Emulators
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

with Interfaces; use Interfaces;

package Emu with
  SPARK_Mode
is

   type Unsigned_4 is mod 16;
   --  4-bit range for case statements over 16-byte ranges

   type Mem_Byte_Range is array (Natural range <>) of Unsigned_8 with
     Alignment => 8;
   --  Type for a range of memory of any size

   Mem_Page_Size : constant := 256;
   --  An 8-bit CPU memory page is 256 bytes

   subtype Mem_Page is Mem_Byte_Range (0 .. Mem_Page_Size - 1);
   --  Type for a 256-byte page of memory

   Mem_Page_Count : constant := 256;
   --  An 8-bit CPU memory bank is 256 pages (64 KB)

   Mem_Bank_Size : constant := Mem_Page_Size * Mem_Page_Count;
   --  An 8-bit CPU memory bank is 256 pages (64 KB)

   subtype Mem_Bank is Mem_Byte_Range (0 .. Mem_Bank_Size - 1);
   --  Type for a 64K bank of memory

end Emu;
