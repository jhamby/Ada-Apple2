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

package Apple2.Joystick with
  SPARK_Mode
is

   procedure Joy_Read_Button
     (C : in out Apple2_Base; ID : Joystick_Button; Value : out Unsigned_8);
   --  Read joystick button (in high bit)

   procedure Joy_Read_Position
     (C : in out Apple2_Base; ID : Joystick_Axis; Value : out Unsigned_8);
   --  Read joystick position (paddle controller active in high bit)

   procedure Joy_Reset_Position (C : in out Apple2_Base);
   --  Reset joystick position

end Apple2.Joystick;
