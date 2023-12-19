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
   -------------------------------
   -- Video_Get_VBL_Bar --
   -------------------------------

   function Video_Get_VBL_Bar (C : Apple2_Base) return Boolean is
   begin
      return C.Scan_Line < Visible_Lines_Per_Frame;
   end Video_Get_VBL_Bar;

   -------------------------------
   -- Video_Get_Scanner_Address --
   -------------------------------

   function Video_Get_Scanner_Address (C : Apple2_Base) return Unsigned_16 is
      Mode      : constant Mode_Flags  := C.Mode;
      Column    : constant Unsigned_16 := C.Column_Cycle;
      Scan_Line : constant Unsigned_16 := C.Scan_Line;

      Hi_Res       : Boolean := Mode.Video_Hi_Res and not Mode.Video_Text;
      Scan_Line_C0 : constant Unsigned_16 := Scan_Line and 16#C0#;
      Address      : Unsigned_16;
   begin

      --  Turn off hi-res mode after scan line 160 in mixed mode
      if (Mode.Video_Hi_Res and Mode.Video_Mixed)
        and then (Scan_Line and 16#A0#) = 16#A0#
      then
         Hi_Res := False;
      end if;

      --  Common:
      --   a0-a2  : low 3 bits of column counter
      --   a3-a6  : sum of 16#0D# + column bits 3-5 + scan line bits 6-7
      --            shifted to fill a3-a4 and a5-a6
      --   a7-a9  : scan line bits 3-5 shifted left 4 bits
      --
      --  Hi-Res:
      --  a10-a12 : scan line bits 0-2 shifted left 10 bits
      --    a13   : Page_2 off or 80_Store on
      --    a14   : Page_2 on and 80_Store off
      --
      --  Text and Lo-Res:
      --    a10   : Page_2 off or 80_Store on
      --    a11   : Page_2 on and 80_Store off
      --    a12   : original Apple ][ sets this bit during HBL (not handled)

      Address :=
        (Column and 16#07#) or
        ((16#68# + (Column and 16#38#) +
          (Shift_Right (Scan_Line_C0, 1) or Shift_Right (Scan_Line_C0, 3))) and
         16#78#) or
        Shift_Left (Scan_Line and 16#38#, 4);

      if Hi_Res then
         Address := Address or Shift_Left (Scan_Line and 16#07#, 10);
         if Mode.Mem_80_Store or not Mode.Video_Page_2 then
            Address := Address or 16#2000#;
         elsif Mode.Video_Page_2 and not Mode.Mem_80_Store then
            Address := Address or 16#4000#;
         end if;
      else
         if Mode.Mem_80_Store or not Mode.Video_Page_2 then
            Address := Address or 16#0400#;
         elsif Mode.Video_Page_2 and not Mode.Mem_80_Store then
            Address := Address or 16#0800#;
         end if;
      end if;

      return Address;
   end Video_Get_Scanner_Address;

end Apple2.Video;
