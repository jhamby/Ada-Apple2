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

   type Mem_Byte_Range is array (Unsigned_32 range <>) of Unsigned_8 with
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

   type CPU_Cycle_Count is mod 2**64;
   --  Unsigned 64-bit type increasing from cycle 0 at start of VBlank

   type File_Op_Status_Type is
     (File_Success, Dir_Not_Found, File_Not_Found, File_Error);
   --  High-level file I/O success status values

   --  Helper functions

   procedure Get_Hex_Digit
     (Char : Character; Digit : in out Unsigned_8; Success : out Boolean);
   --  Try to parse a character as a hexadecimal digit

   procedure Get_Hex_Byte
     (Chars : String; Digit : in out Unsigned_8; Success : out Boolean) with
     Pre => Chars'Length = 2;
   --  Try to parse two characters as a hexadecimal byte

   procedure Put_Hex_Byte (Chars : in out String; Digit : Unsigned_8) with
     Pre => Chars'Length = 2;
   --  Print a hex byte to the specified two character String slice

end Emu;
