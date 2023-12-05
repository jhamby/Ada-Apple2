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

package body Apple2.Serial with
  SPARK_Mode
is

   --------------
   -- SSC_Read --
   --------------

   procedure SSC_Read
     (C          : in out Apple2_Base; Address : Unsigned_16;
      Read_Value :    out Unsigned_8; Cycles_Left : Natural)
   is
      pragma Unreferenced (Address, Cycles_Left, C);
   begin
      Read_Value := 0;  --  TODO: add implementation
   end SSC_Read;

   ---------------
   -- SSC_Write --
   ---------------

   procedure SSC_Write
     (C : in out Apple2_Base; Address : Unsigned_16; Write_Value : Unsigned_8;
      Cycles_Left :        Natural)
   is
   begin
      null;  --  TODO: add implementation
   end SSC_Write;

end Apple2.Serial;
