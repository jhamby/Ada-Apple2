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

package body WDC_CPU_65C02 with
  SPARK_Mode
is

   ---------------------------
   -- CPU_Execute_WDC_65C02 --
   ---------------------------

   procedure CPU_Execute_WDC_65C02
     (C : in out CPU_6502_Series; Mem : not null access RAM_All_Banks;
      Column_Cycle, Scan_Line : in out Unsigned_16;
      Num_Columns, Num_Lines  :        Unsigned_16)
   is
   begin
      null;  --  TODO: add implementation
   end CPU_Execute_WDC_65C02;

end WDC_CPU_65C02;
