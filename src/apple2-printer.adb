--  AppleWin : An Apple //e emulator for Windows
--
--  Copyright (C) 1994-1996, Michael O'Brien
--  Copyright (C) 1999-2001, Oliver Schmidt
--  Copyright (C) 2002-2005, Tom Charlesworth
--  Copyright (C) 2006-2007, Tom Charlesworth, Michael Pohoreski, Nick Westgate
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

package body Apple2.Printer with
  SPARK_Mode
is

   procedure Check_Print (C : Apple2_Base; Success : out Boolean);
   --  Create and open the printer file and return status

   -----------------
   -- Print_Status --
   -----------------

   procedure Print_Status (C : Apple2_Base; Value : out Unsigned_8) is
      Ignore : Boolean;
   begin
      Check_Print (C, Ignore);
      Value := 16#FF#;  --  TODO: status
      Put_Line ("Print_Status called");
   end Print_Status;

   --------------------
   -- Print_Transmit --
   --------------------

   procedure Print_Transmit (C : Apple2_Base; Value : Unsigned_8) is
      pragma Unreferenced (C);
   begin
      Put_Line ("Print_Transmit - Value: " & Value'Image);
      --  Real code will go here
   end Print_Transmit;

   -----------------
   -- Check_Print --
   -----------------

   procedure Check_Print (C : Apple2_Base; Success : out Boolean) is
      pragma Unreferenced (C);
   begin
      Success := True;  --  TODO: add implementation
   end Check_Print;

end Apple2.Printer;
