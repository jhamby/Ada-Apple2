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

with WDC_CPU_65C02; use WDC_CPU_65C02;

package body Apple2 with
  SPARK_Mode
is

   --------------
   -- Mem_Read --
   --------------

   function Mem_Read
     (Mem     : not null access constant RAM_All_Banks; Bank : RAM_Bank_Index;
      Address : Unsigned_16) return Unsigned_8
   is
   begin
      return Mem (Natural (Bank) * Mem_Bank_Size + Natural (Address));
   end Mem_Read;

   ---------------
   -- Mem_Write --
   ---------------

   procedure Mem_Write
     (C    : in out Apple2_Base; Mem : not null access RAM_All_Banks;
      Bank :        RAM_Bank_Index; Address : Unsigned_16; Value : Unsigned_8)
   is
   begin
      Mem (Natural (Bank) * Mem_Bank_Size + Natural (Address)) := Value;
      C.Page_Clean_Flags (Unsigned_8 (Shift_Right (Address, 8) and 16#FF#)) :=
        Page_Flag_Reset;
      --  Clear page clean flag so video can refresh any changed regions
   end Mem_Write;

   ---------------
   -- Is_Apple2 --
   ---------------

   function Is_Apple2 (C : Apple2_Base) return Boolean is
   begin
      return C.Model < Apple_2e;
   end Is_Apple2;

   -----------------------
   -- Get_Apple_2_Model --
   -----------------------

   function Get_Apple_2_Model (C : Apple2_Base) return Apple_2_Model is
   begin
      return C.Model;
   end Get_Apple_2_Model;

   -----------------------
   -- Set_Apple_2_Model --
   -----------------------

   procedure Set_Apple_2_Model
     (C : in out Apple2_Base; New_Type : Apple_2_Model)
   is
   begin
      C.Model := New_Type;
   end Set_Apple_2_Model;

   -----------------
   -- CPU_Execute --
   -----------------

   procedure CPU_Execute
     (C            : in out Apple2_Base; Mem : not null access RAM_All_Banks;
      Total_Cycles :        Natural)
   is
   begin
      if C.Model = Apple_2e_Enhanced then
         CPU_Execute_WDC_65C02 (CPU_6502_Series (C), Mem, Total_Cycles);
      else
         CPU_Execute_MOS_6502 (CPU_6502_Series (C), Mem, Total_Cycles);
      end if;
   end CPU_Execute;

end Apple2;
