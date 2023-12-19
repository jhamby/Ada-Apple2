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

with Ada.Text_IO; use Ada.Text_IO;

package body Apple2.Speaker with
  SPARK_Mode
is
   Last_Debug_Print_Time : CPU_Cycle_Count := 0;
   --  Keep track of time last printed so we don't flood stdout

   -----------------
   -- Spkr_Toggle --
   -----------------

   procedure Spkr_Toggle (C : in out Apple2_Base; Is_Write : Boolean) is
   begin
      if C.Spkr_First_Click_Cycle = 0 then
         C.Spkr_First_Click_Cycle := C.Cycles_Since_Boot;
      end if;

      C.Spkr_Last_Click_Cycle := C.Cycles_Since_Boot;
      C.Spkr_Num_Clicks       := C.Spkr_Num_Clicks + 1;

      --  TODO: should we treat writes the same as reads for audio purposes?
      if Is_Write then
         if C.Cycles_Since_Boot - Last_Debug_Print_Time >= 1E6 then
            Put_Line ("write to speaker I/O address");
            Last_Debug_Print_Time := C.Cycles_Since_Boot;
         end if;
      end if;

      --  Real code will go here
   end Spkr_Toggle;

end Apple2.Speaker;
