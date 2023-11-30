pragma SPARK_Mode;

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

package body Apple2.Video is

   type Video_Flag_Type is mod 2**16;

   Vid_Flag_No_Flags    : constant Video_Flag_Type := 16#0000#;
   Vid_Flag_80_Column   : constant Video_Flag_Type := 16#0001#;
   Vid_Flag_Dbl_Hires   : constant Video_Flag_Type := 16#0002#;
   pragma Unreferenced (Vid_Flag_Dbl_Hires);
   Vid_Flag_Hires       : constant Video_Flag_Type := 16#0004#;
   pragma Unreferenced (Vid_Flag_Hires);
   Vid_Flag_Mask_2      : constant Video_Flag_Type := 16#0008#;
   pragma Unreferenced (Vid_Flag_Mask_2);
   Vid_Flag_Mixed       : constant Video_Flag_Type := 16#0010#;
   pragma Unreferenced (Vid_Flag_Mixed);
   Vid_Flag_Page_2      : constant Video_Flag_Type := 16#0020#;
   pragma Unreferenced (Vid_Flag_Page_2);
   Vid_Flag_Text        : constant Video_Flag_Type := 16#0040#;
   pragma Unreferenced (Vid_Flag_Text);

   Video_Mode : constant Video_Flag_Type := Vid_Flag_No_Flags;
   --  Current video mode

   --------------------------
   -- Video_Mode_80_Column --
   --------------------------

   function Video_Mode_80_Column return Boolean is
   begin
      return (Video_Mode and Vid_Flag_80_Column) /= 0;
   end Video_Mode_80_Column;

   --------------------
   -- Video_Set_Mode --
   --------------------

   procedure Video_Set_Mode (Address : Address_16_Bit) is
   begin
      null;  --  TODO: add implementation
   end Video_Set_Mode;

   ---------------------
   -- Video_Check_VBL --
   ---------------------

   procedure Video_Check_VBL (Read_Value : out Value_8_Bit;
                              Cycles_Left : Natural) is
      pragma Unreferenced (Cycles_Left);
   begin
      Read_Value := 0;  --  TODO: add implementation
   end Video_Check_VBL;

   ----------------------
   -- Video_Check_Mode --
   ----------------------

   procedure Video_Check_Mode (Address : Address_16_Bit;
                               Read_Value : out Value_8_Bit;
                               Cycles_Left : Natural) is
      pragma Unreferenced (Address, Cycles_Left);
   begin
      Read_Value := 0;  --  TODO: add implementation
   end Video_Check_Mode;

   -------------------------------
   -- Video_Get_Scanner_Address --
   -------------------------------

   procedure Video_Get_Scanner_Address (Executed_Cycles : Natural;
                                        VBL_Bar : out Boolean;
                                        Address : out Address_16_Bit) is
      pragma Unreferenced (Executed_Cycles);
   begin
      VBL_Bar := False;  --  TODO: add implementation
      Address := 0;
   end Video_Get_Scanner_Address;

end Apple2.Video;
