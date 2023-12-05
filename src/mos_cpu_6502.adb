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

package body MOS_CPU_6502 with
  SPARK_Mode
is

   --------------------
   -- CPU_Initialize --
   --------------------

   procedure CPU_Initialize (C : in out CPU_6502_Series) is
   begin
      null;  --  TODO: add implementation
   end CPU_Initialize;

   ---------------
   -- CPU_Reset --
   ---------------

   procedure CPU_Reset (C : in out CPU_6502_Series) is
   begin
      null;  --  TODO: add implementation
   end CPU_Reset;

   ---------------------
   -- CPU_Calc_Cycles --
   ---------------------

   procedure CPU_Calc_Cycles
     (C : in out CPU_6502_Series; Cycles_Left : Natural)
   is
   begin
      null;  --  TODO: add implementation
   end CPU_Calc_Cycles;

   --------------------------
   -- CPU_Execute_MOS_6502 --
   --------------------------

   procedure CPU_Execute_MOS_6502
     (C : in out CPU_6502_Series; Mem : not null access RAM_All_Banks;
      Total_Cycles :        Natural)
   is
   begin
      null;  --  TODO: add implementation
   end CPU_Execute_MOS_6502;

end MOS_CPU_6502;
