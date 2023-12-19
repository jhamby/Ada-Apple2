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

package body Apple2.Disk with
  SPARK_Mode
is
   Last_Debug_Print_Time : CPU_Cycle_Count := 0;
   --  Keep track of time last printed so we don't flood stdout

   Debug_Print_Count : Natural := 0;
   --  Number of debug lines printed in the past second

   --------------------
   -- Disk_IO_Access --
   --------------------

   procedure Disk_IO_Access
     (C        : Apple2_Base; Address : Unsigned_16; Value : in out Unsigned_8;
      Is_Write : Boolean)
   is
      Address_String : String (1 .. 4) := (others => ' ');
      Value_String   : String (1 .. 2) := (others => ' ');
   begin
      if C.Cycles_Since_Boot - Last_Debug_Print_Time >= 1E6 then
         --  Reset the debug output counter
         Last_Debug_Print_Time := C.Cycles_Since_Boot;
         Debug_Print_Count     := 0;
      end if;

      if Debug_Print_Count <= 100 then
         --  debug output
         Debug_Print_Count := Debug_Print_Count + 1;
         Put_Hex_Byte
           (Address_String (1 .. 2), Unsigned_8 (Shift_Right (Address, 8)));
         Put_Hex_Byte
           (Address_String (3 .. 4), Unsigned_8 (Address and 16#FF#));
         Put_Hex_Byte (Value_String, Value);
         Put_Line
           ("Disk_IO_Access - Address: $" & Address_String & " Value: $" &
            Value_String & " Mode: " & (if Is_Write then "Write" else "Read"));
      end if;

      --  Real code will go here
      Value := 0;
   end Disk_IO_Access;

end Apple2.Disk;
