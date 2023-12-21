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

with Apple2.Memory; use Apple2.Memory;

package Apple2.Benchmark with
  SPARK_Mode
is

   procedure CPU_Setup_Benchmark
     (C : in out Computer; Mem : not null access RAM_All_Banks);
   --  Create code segments of commonly-used opcodes in $0300 .. $03FF

   procedure CPU_Run_Benchmark
     (C : in out Computer; Mem : not null access RAM_All_Banks);
   --  Count the video frames of CPU that we can emulate in one second

   procedure Benchmark_Error (PC : Unsigned_16; Cycle : CPU_Cycle_Count);
   --  Print an error message if PC goes out of the expected address range

end Apple2.Benchmark;
