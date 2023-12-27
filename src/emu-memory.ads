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

package Emu.Memory with
  SPARK_Mode
is

   RAM_Works_Banks : constant := 128;
   --  Always use the largest RAMWorks III size (128 x 64K pages = 8 MB)

   RAM_Total_Banks : constant := RAM_Works_Banks + 2;
   --  0       = 64K main RAM
   --  1       = 64K of ROMs (system + peripheral)
   --  2       = aux 64K: //e Extended 80 Col Card, //c, or RAMWorks III
   --  2 - 129 = RAMWorks III banks (128 banks including aux 64K bank 1)

   type RAM_Bank_Index is range 0 .. RAM_Total_Banks - 1;
   --  Define a type for the RAM / ROM bank index

   RAM_Bank_Main : constant RAM_Bank_Index := 0;
   --  bank 0 = 64K main RAM

   RAM_Bank_ROMs : constant RAM_Bank_Index := 1;
   --  bank 1 = 64K of ROMs

   RAM_Bank_Aux_Start : constant RAM_Bank_Index := 2;
   --  bank 2 = aux 64K: //e Extended 80 Col Card, //c, or RAMWorks III

   RAM_All_Banks_Size : constant := RAM_Total_Banks * Mem_Bank_Size;
   --  total size in bytes of the RAM / ROM byte array

   subtype RAM_All_Banks is Mem_Byte_Range (0 .. RAM_All_Banks_Size - 1);
   --  Define a type for the RAM / ROM byte array

end Emu.Memory;
