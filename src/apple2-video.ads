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

package Apple2.Video is

   function Video_Mode_80_Column return Boolean;
   --  Is the video in 80 column mode?

   procedure Video_Set_Mode (Address : Address_16_Bit);
   --  Set the video mode based on memory address accessed

   procedure Video_Check_VBL (Read_Value : out Value_8_Bit;
                              Cycles_Left : Natural);
   --  Return byte with keycode OR'd with VBL status in high bit

   procedure Video_Check_Mode (Address : Address_16_Bit;
                               Read_Value : out Value_8_Bit;
                               Cycles_Left : Natural);
   --  Return byte with keycode OR'd with video mode in high bit.
   --  For address $xx7F, return floating bus OR'd with double hires mode.

   procedure Video_Get_Scanner_Address (Executed_Cycles : Natural;
                                        VBL_Bar : out Boolean;
                                        Address : out Address_16_Bit);
   --  Get video scanner address and VBL' state

end Apple2.Video;
