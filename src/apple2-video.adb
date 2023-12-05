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

package body Apple2.Video with
  SPARK_Mode
is

   --------------------------
   -- Video_Mode_80_Column --
   --------------------------

   function Video_Mode_80_Column (C : Apple2_Base) return Boolean is
   begin
      return (C.Video_Mode and Vid_Flag_80_Column) /= 0;
   end Video_Mode_80_Column;

   --------------------
   -- Video_Set_Mode --
   --------------------

   procedure Video_Set_Mode (C : in out Apple2_Base; Address : Unsigned_16) is
      pragma Unreferenced (C, Address);
   begin
      null;  --  TODO: add implementation
   end Video_Set_Mode;

   ---------------------
   -- Video_Check_VBL --
   ---------------------

   procedure Video_Check_VBL
     (C           : in out Apple2_Base; Read_Value : out Unsigned_8;
      Cycles_Left :        Natural)
   is
      pragma Unreferenced (Cycles_Left, C);
   begin
      Read_Value := 0;  --  TODO: add implementation
   end Video_Check_VBL;

   ----------------------
   -- Video_Check_Mode --
   ----------------------

   procedure Video_Check_Mode
     (C          : in out Apple2_Base; Address : Unsigned_16;
      Read_Value :    out Unsigned_8; Cycles_Left : Natural)
   is
      pragma Unreferenced (Address, Cycles_Left, C);
   begin
      Read_Value := 0;  --  TODO: add implementation
   end Video_Check_Mode;

   -------------------------------
   -- Video_Get_Scanner_Address --
   -------------------------------

   procedure Video_Get_Scanner_Address
     (C : in out Apple2_Base; VBL_Bar : out Boolean; Bank : out RAM_Bank_Index;
      Address :    out Unsigned_16; Executed_Cycles : Natural)
   is
      pragma Unreferenced (Executed_Cycles, C);
   begin
      VBL_Bar := False;  --  TODO: add implementation
      Bank    := 0;
      Address := 0;
   end Video_Get_Scanner_Address;

end Apple2.Video;
