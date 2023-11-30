pragma SPARK_Mode;

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

with Apple2.CPU;       use Apple2.CPU;

package body Apple2.Mockingboard is

   ---------------
   -- Phasor_IO --
   ---------------

   procedure Phasor_IO (Address : Address_16_Bit) is
   begin
      null;
      --  TODO: add implementation
   end Phasor_IO;

   -------------
   -- MB_Read --
   -------------

   procedure MB_Read
     (Address : Address_16_Bit; Read_Value : out Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (Address);
   begin
      CPU_Calc_Cycles (Cycles_Left);
      Read_Value := 0;
      --  TODO: add implementation
   end MB_Read;

   --------------
   -- MB_Write --
   --------------

   procedure MB_Write
     (Address : Address_16_Bit; Write_Value : Value_8_Bit;
      Cycles_Left : Natural) is
      pragma Unreferenced (Address, Write_Value);
   begin
      CPU_Calc_Cycles (Cycles_Left);
      --  TODO: add implementation
   end MB_Write;

end Apple2.Mockingboard;
