--  AppleWin : An Apple //e emulator for Windows
--
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

package Apple2.Random with
  SPARK_Mode
is

   procedure Reset_Generator;
   --  Seed the PRNG for Mem_Read_Random_Data

   function Mem_Read_Random_Data (High_Bit : Boolean) return Unsigned_8;
   --  Read random bus data and replace high bit with specified value
   --  Called by Disk ][ I/O only. Doesn't use machine state.

end Apple2.Random;
