--  AppleWin : An Apple //e emulator for Windows
--
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

with Ada.Numerics.Discrete_Random;

package body Apple2.Random with
  SPARK_Mode => Off
is

   package Random_Byte is new Ada.Numerics.Discrete_Random (Unsigned_8);
   --  Instantiate Discrete_Random package for unsigned 8-bit values

   Random_Generator : Random_Byte.Generator;
   --  Generate random bytes for Disk ][ emulation (not part of machine state)

   ---------------------
   -- Reset_Generator --
   ---------------------

   procedure Reset_Generator is
   begin
      Random_Byte.Reset (Random_Generator);
      --  Seed PRNG for Mem_Read_Random_Data
   end Reset_Generator;

   --------------------------
   -- Mem_Read_Random_Data --
   --------------------------

   function Mem_Read_Random_Data (High_Bit : Boolean) return Unsigned_8 is
      Ret_Values : constant Mem_Byte_Range (0 .. 15) :=
        (16#00#, 16#2D#, 16#2D#, 16#30#, 16#30#, 16#32#, 16#32#, 16#34#,
         16#35#, 16#39#, 16#43#, 16#43#, 16#43#, 16#60#, 16#7F#, 16#7F#);
      Value      : Unsigned_8 := Random_Byte.Random (Random_Generator);
   begin
      if Value <= 170 then
         if High_Bit then
            return 16#A0#;
         else
            return 16#20#;
         end if;
      else
         Value := Ret_Values (Integer (Value and 15));
         if High_Bit then
            return Value or 16#80#;
         else
            return Value;
         end if;
      end if;
   end Mem_Read_Random_Data;

end Apple2.Random;
