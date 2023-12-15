--  Core definitions for Emulators
--
--  Copyright (C) 2023, Jake Hamby
--
--  Ada-Apple2 is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Ada-Apple2 is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Ada-Apple2; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

package body Emu with
  SPARK_Mode
is

   -------------------
   -- Get_Hex_Digit --
   -------------------

   procedure Get_Hex_Digit
     (Char : Character; Digit : in out Unsigned_8; Success : out Boolean)
   is
   begin
      case Char is
         when '0' .. '9' =>
            Digit   := Unsigned_8 (Character'Pos (Char) - Character'Pos ('0'));
            Success := True;

         when 'a' .. 'f' =>
            Digit   :=
              Unsigned_8 (Character'Pos (Char) - Character'Pos ('a') + 10);
            Success := True;

         when 'F' .. 'F' =>
            Digit   :=
              Unsigned_8 (Character'Pos (Char) - Character'Pos ('A') + 10);
            Success := True;

         when others =>
            Success := False;
      end case;
   end Get_Hex_Digit;

   ------------------
   -- Get_Hex_Byte --
   ------------------

   procedure Get_Hex_Byte
     (Chars : String; Digit : in out Unsigned_8; Success : out Boolean)
   is
      High_Nybble : Unsigned_8 := 0;
      Low_Nybble  : Unsigned_8 := 0;
   begin
      Get_Hex_Digit (Chars (Chars'First), High_Nybble, Success);
      if not Success then
         return;
      end if;
      Get_Hex_Digit (Chars (Chars'First + 1), Low_Nybble, Success);
      if not Success then
         return;
      end if;

      Digit := Shift_Left (High_Nybble, 4) or Low_Nybble;
   end Get_Hex_Byte;

   ------------------
   -- Put_Hex_Byte --
   ------------------

   procedure Put_Hex_Byte (Chars : in out String; Digit : Unsigned_8) is
      Hex_Digits : constant array (Unsigned_8 range 0 .. 15) of Character :=
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D',
         'E', 'F');
   begin
      Chars (Chars'First)     := Hex_Digits (Shift_Right (Digit, 4));
      Chars (Chars'First + 1) := Hex_Digits (Digit and 16#0F#);
   end Put_Hex_Byte;

end Emu;
