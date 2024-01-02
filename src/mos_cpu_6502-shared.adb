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

--  Description: 6502/65C02 emulation
--  Author: Various

--  TeaRex's Note about illegal opcodes:
--  ------------------------------------
--  . I've followed the names and descriptions given in
--  . "Extra Instructions Of The 65XX Series CPU"
--  . by Adam Vardy, dated Sept 27, 1996.
--  . The exception is, what he calls "SKB" and "SKW" I call "NOP",
--  . for consistency's sake. Several other naming conventions exist.
--  . Of course, only the 6502 has illegal opcodes, the 65C02 doesn't.
--  . Thus they're not emulated in Enhanced //e g_nAppMode. Games relying on
--  . them don't run on a real Enhanced //e either. The old mixture of 65C02
--  . emulation and skipping the right number of bytes for illegal 6502
--  . opcodes, while working surprisingly well in practice, was IMHO
--  . ill-founded in theory and has thus been removed.

--  Note about Check_Page_Change:
--  -------------------
--  This is used to determine if a cycle needs to be added for a page-crossing.
--
--  Modes that are affected:
--  . ABS,X; ABS,Y; (IND),Y
--
--  The following opcodes (when indexed) add a cycle if page is crossed:
--  . ADC, AND, Bxx, CMP, EOR, LDA, LDX, LDY, ORA, SBC
--  . NB. Those opcode that DO NOT write to memory.
--  . 65C02: JMP (ABS-INDIRECT): 65C02 fixes JMP ($xxFF) bug but needs extra
--                               cycle in that case
--  . 65C02: JMP (ABS-INDIRECT,X): Probably. Currently unimplemented.
--
--  The following opcodes (when indexed) DO NOT add a cycle if page is crossed:
--  . ASL, DEC, INC, LSR, ROL, ROR, STA, STX, STY
--  . NB. Those opcode that DO write to memory.
--
--  What about these:
--  . 65C02: STZ?, TRB?, TSB?
--  . Answer: TRB & TSB don't have affected adressing modes
--  .         STZ probably doesn't add a cycle since otherwise it would be
--            slower than STA which doesn't make sense.
--
--  NB. 'Zero-page indexed' opcodes wrap back to zero-page.
--  .   The same goes for all the zero-page indirect modes.
--
--  NB2. bSlowerOnPagecross can't be used for r/w detection, as these
--  .    opcodes don't init this flag:
--  . $EC CPX ABS (since there's no addressing g_nAppMode of CPY which has
--                 variable cycle number)
--  . $CC CPY ABS (same)
--
--  65C02 info:
--  .  Read-modify-write instructions abs indexed in same page take 6 cycles
--     (cf. 7 cycles for 6502)
--  .  ASL, DEC, INC, LSR, ROL, ROR
--  .  This should work now
--
--  Thanks to Scott Hemphill for the verified CMOS ADC and SBC algorithm!
--  You rock. And thanks to the VICE team for the NMOS ADC and SBC algorithms
--  as well as the algorithms for those illops which involve ADC or SBC.
--  You rock too.

package body MOS_CPU_6502.Shared with
  SPARK_Mode
is
   -----------------------
   -- Helper procedures --
   -----------------------

   procedure Pop
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; Value : out Unsigned_8)
   is
   begin
      SP := SP + 1;  --  wraps from $FF to $00
      Mem_IO_Read (C, Mem, 16#0100# + Unsigned_16 (SP), Value);
   end Pop;

   procedure Push
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; Value : Unsigned_8)
   is
   begin
      Mem_IO_Write (C, Mem, 16#0100# + Unsigned_16 (SP), Value);
      SP := SP - 1;  --  wraps from $00 to $FF
   end Push;

   procedure Load_Flags
     (P : CPU_6502_P; Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD : out Boolean)
   is
   begin
      Flag_C   := (P and P_Flag_Carry) /= 0;
      Flag_N   := (P and P_Flag_Negative) /= 0;
      Flag_V   := (P and P_Flag_Overflow) /= 0;
      Flag_Z   := (P and P_Flag_Zero) /= 0;
      Flag_BCD := (P and P_Flag_Decimal_Mode) /= 0;
   end Load_Flags;

   procedure Save_Flags
     (P                                        : in out CPU_6502_P;
      Flag_C, Flag_N, Flag_V, Flag_Z, Flag_BCD :        Boolean)
   is
   begin
      P :=
        (P and P_Flag_CNVZD_Mask) or (if Flag_C then P_Flag_Carry else 0) or
        (if Flag_N then P_Flag_Negative else 0) or
        (if Flag_V then P_Flag_Overflow else 0) or
        (if Flag_Z then P_Flag_Zero else 0) or
        (if Flag_BCD then P_Flag_Decimal_Mode else 0);
   end Save_Flags;

   procedure Set_NZ (Value : Unsigned_8; Flag_N, Flag_Z : out Boolean) is
   begin
      Flag_N := (Value and 16#80#) /= 0;
      Flag_Z := (Value = 0);
   end Set_NZ;

   procedure Set_Z (Value : Unsigned_8; Flag_Z : out Boolean) is
   begin
      Flag_Z := (Value = 0);
   end Set_Z;

   procedure Branch_Taken (PC : in out Unsigned_16; Distance : Unsigned_8) is
      Distance_16 : Unsigned_16;
   begin
      --  sign-extend relative distance to use two's-complement addition
      if (Distance and 16#80#) /= 0 then
         Distance_16 := Unsigned_16 (Distance) or 16#FF00#;
      else
         Distance_16 := Unsigned_16 (Distance);
      end if;

      PC := PC + Distance_16;
   end Branch_Taken;

   procedure Branch_Taken
     (PC           : in out Unsigned_16; Distance : Unsigned_8;
      Extra_Cycles :    out Unsigned_8)
   is
      Base : constant Unsigned_16 := PC;
   begin
      Branch_Taken (PC, Distance);
      if ((Base xor PC) and 16#FF00#) /= 0 then
         Extra_Cycles := 2;
      else
         Extra_Cycles := 1;
      end if;
   end Branch_Taken;

   procedure Check_Page_Change
     (Base, PC : Unsigned_16; Extra_Cycles : out Unsigned_8)
   is
   begin
      if ((Base xor PC) and 16#FF00#) /= 0 then
         Extra_Cycles := 1;
      else
         Extra_Cycles := 0;
      end if;
   end Check_Page_Change;

   procedure Get_Word
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Word : out Unsigned_16)
   is
      Low_Byte, High_Byte : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Low_Byte);
      Mem_IO_Read (C, Mem, Address + 1, High_Byte);
      Word :=
        Unsigned_16 (Low_Byte) or Shift_Left (Unsigned_16 (High_Byte), 8);
   end Get_Word;

   procedure Get_Word_FF_Wrap
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Word : out Unsigned_16)
   is
      Low_Byte, High_Byte : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Low_Byte);
      if (Address and 16#FF#) = 16#FF# then
         Mem_IO_Read (C, Mem, Address and 16#FF00#, High_Byte);
      else
         Mem_IO_Read (C, Mem, Address + 1, High_Byte);
      end if;
      Word :=
        Unsigned_16 (Low_Byte) or Shift_Left (Unsigned_16 (High_Byte), 8);
   end Get_Word_FF_Wrap;

   ----------------------
   -- Addressing modes --
   ----------------------

   procedure Mode_ABS
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16)
   is
   begin
      Get_Word (C, Mem, PC, Address);
      PC := PC + 2;
   end Mode_ABS;

   procedure Mode_IABSX
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; X : Unsigned_8)
   is
      Base : Unsigned_16;
   begin
      Get_Word (C, Mem, PC, Base);
      Base := Base + Unsigned_16 (X);
      Get_Word (C, Mem, Base, Address);
      PC := PC + 2;
   end Mode_IABSX;

   procedure Mode_ABSX
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; X : Unsigned_8)
   is
      Base : Unsigned_16;
   begin
      Get_Word (C, Mem, PC, Base);
      Address := Base + Unsigned_16 (X);
      PC      := PC + 2;
   end Mode_ABSX;

   procedure Mode_ABSX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; X : Unsigned_8;
      Extra_Cycles :    out Unsigned_8)
   is
      Base : Unsigned_16;
   begin
      Get_Word (C, Mem, PC, Base);
      Address := Base + Unsigned_16 (X);
      PC      := PC + 2;
      Check_Page_Change (Base, Address, Extra_Cycles);
   end Mode_ABSX;

   procedure Mode_ABSY
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; Y : Unsigned_8)
   is
      Base : Unsigned_16;
   begin
      Get_Word (C, Mem, PC, Base);
      Address := Base + Unsigned_16 (Y);
      PC      := PC + 2;
   end Mode_ABSY;

   procedure Mode_ABSY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; Y : Unsigned_8;
      Extra_Cycles :    out Unsigned_8)
   is
      Base : Unsigned_16;
   begin
      Get_Word (C, Mem, PC, Base);
      Address := Base + Unsigned_16 (Y);
      PC      := PC + 2;
      Check_Page_Change (Base, Address, Extra_Cycles);
   end Mode_ABSY;

   procedure Mode_IABSCMOS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC           : in out Unsigned_16; Address : out Unsigned_16;
      Extra_Cycles :    out Unsigned_8)
   is
      Base : constant Unsigned_16 := PC;
   begin
      Get_Word (C, Mem, PC, Address);
      PC           := PC + 2;
      Extra_Cycles := (if (Base and 16#FF#) = 16#FF# then 1 else 0);
   end Mode_IABSCMOS;

   procedure Mode_IABSNMOS
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16)
   is
   begin
      Get_Word_FF_Wrap (C, Mem, PC, Address);
      PC := PC + 2;
   end Mode_IABSNMOS;

   procedure Mode_IMM (PC : in out Unsigned_16; Address : out Unsigned_16) is
   begin
      Address := PC;
      PC      := PC + 1;
   end Mode_IMM;

   procedure Mode_INDX
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; X : Unsigned_8)
   is
      ZP_Base : constant Unsigned_16 := (PC + Unsigned_16 (X)) and 16#FF#;
   begin
      Get_Word_FF_Wrap (C, Mem, ZP_Base, Address);
      PC := PC + 1;
   end Mode_INDX;

   procedure Mode_INDY
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; Y : Unsigned_8)
   is
      Base : Unsigned_16;
   begin
      Get_Word_FF_Wrap (C, Mem, PC, Base);
      Address := Base + Unsigned_16 (Y);
      PC      := PC + 1;
   end Mode_INDY;

   procedure Mode_INDY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; Y : Unsigned_8;
      Extra_Cycles :    out Unsigned_8)
   is
      Base : Unsigned_16;
   begin
      Get_Word_FF_Wrap (C, Mem, PC, Base);
      Address := Base + Unsigned_16 (Y);
      PC      := PC + 1;
      Check_Page_Change (Base, Address, Extra_Cycles);
   end Mode_INDY;

   procedure Mode_IZPG
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16)
   is
   begin
      Get_Word_FF_Wrap (C, Mem, PC, Address);
      PC := PC + 1;
   end Mode_IZPG;

   procedure Mode_REL
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Distance : out Unsigned_8)
   is
   begin
      Mem_IO_Read (C, Mem, PC, Distance);
      PC := PC + 1;
   end Mode_REL;

   procedure Mode_ZPG
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16)
   is
      Value : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, PC, Value);
      Address := Unsigned_16 (Value);
      PC      := PC + 1;
   end Mode_ZPG;

   procedure Mode_ZPGX
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; X : Unsigned_8)
   is
      Value : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, PC, Value);
      Address := Unsigned_16 (Value + X);  --  Unsigned_8 will stay in $00xx
      PC      := PC + 1;
   end Mode_ZPGX;

   procedure Mode_ZPGY
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; Address : out Unsigned_16; Y : Unsigned_8)
   is
      Value : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, PC, Value);
      Address := Unsigned_16 (Value + Y);  --  Unsigned_8 will stay in $00xx
      PC      := PC + 1;
   end Mode_ZPGY;

   ------------------
   -- Instructions --
   ------------------

   procedure Op_ADC_NMOS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean)
   is
      Temp : Unsigned_8;
      Val  : Unsigned_16;
   begin
      Mem_IO_Read (C, Mem, Address, Temp);
      if Flag_Decimal then
         Val :=
           Unsigned_16
             ((A and 16#0F#) + (Temp and 16#0F#) + (if Flag_C then 1 else 0));

         if Val > 16#09# then
            Val := Val + 16#06#;
         end if;

         Flag_C := (Val > 16#0F#);  --  carry flag for low digit

         Val :=
           (Val and 16#0F#) + Unsigned_16 (A and 16#F0#) +
           Unsigned_16 (Temp and 16#F0#) + (if Flag_C then 16#10# else 0);

         --  NMOS 6502 incorrectly sets flags as if non-decimal mode
         Flag_Z := (A + Temp + (if Flag_C then 1 else 0)) = 0;
         Flag_N := (Val and 16#80#) /= 0;

         Flag_V :=
           ((A and 16#80#) = (Temp and 16#80#)) and
           ((A and 16#80#) /= Unsigned_8 (Val and 16#80#));

         if (Val and 16#01F0#) > 16#90# then
            Val := Val + 16#60#;
         end if;

         Flag_C := (Val and 16#0FF0#) > 16#F0#;
         A      := Unsigned_8 (Val and 16#FF#);
      else
         Val :=
           Unsigned_16 (A) + Unsigned_16 (Temp) + (if Flag_C then 1 else 0);

         Flag_C := Val > 16#FF#;

         Flag_V :=
           ((A and 16#80#) = (Temp and 16#80#)) and
           ((A and 16#80#) /= Unsigned_8 (Val and 16#80#));

         A := Unsigned_8 (Val and 16#FF#);
         Set_NZ (A, Flag_N, Flag_Z);
      end if;
   end Op_ADC_NMOS;

   procedure Op_ADC_CMOS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean;
      Extra_Cycles           : in out Unsigned_8)
   is
      Temp : Unsigned_8;
      Val  : Unsigned_16;
   begin
      Mem_IO_Read (C, Mem, Address, Temp);

      Flag_V := ((A and 16#80#) = (Temp and 16#80#));

      if Flag_Decimal then
         Extra_Cycles := Extra_Cycles + 1;

         Val :=
           Unsigned_16
             ((A and 16#0F#) + (Temp and 16#0F#) + (if Flag_C then 1 else 0));

         Flag_C := (Val > 16#0F#);  --  carry flag for low digit

         Val :=
           (Val and 16#0F#) + Unsigned_16 (A and 16#F0#) +
           Unsigned_16 (Temp and 16#F0#) + (if Flag_C then 16#10# else 0);

         if Val >= 16#A0# then
            Flag_C := True;
            if Val >= 16#0180# then
               Flag_V := False;
            end if;
            Val := Val + 16#60#;
         else
            Flag_C := False;
            if Val < 16#80# then
               Flag_V := False;
            end if;
         end if;
      else
         Val :=
           Unsigned_16 (A) + Unsigned_16 (Temp) + (if Flag_C then 1 else 0);

         if Val >= 16#0100# then
            Flag_C := True;
            if Val >= 16#0180# then
               Flag_V := False;
            end if;
            Val := Val + 16#60#;
         else
            Flag_C := False;
            if Val < 16#80# then
               Flag_V := False;
            end if;
         end if;
      end if;

      A := Unsigned_8 (Val and 16#FF#);
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_ADC_CMOS;

   procedure Op_ALR
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : in out Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      A      := A and Val;
      Flag_C := (A and 1) /= 0;
      Flag_N := False;
      A      := Shift_Right (A, 1);
      Set_Z (A, Flag_Z);
   end Op_ALR;

   procedure Op_AND
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; A : in out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      A := A and Val;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_AND;

   procedure Op_ANC
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : in out Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      A := A and Val;
      Set_NZ (A, Flag_N, Flag_Z);
      Flag_C := Flag_N;
   end Op_ANC;

   procedure Op_ARR
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean)
   is
      Temp, Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Temp);
      Temp := Temp and A;

      Val := Shift_Right (Temp, 1) or (if Flag_C then 16#80# else 0);

      if Flag_Decimal then
         Flag_N := Flag_C;
         Set_Z (Val, Flag_Z);

         Flag_V := ((Val and 16#40#) /= (Temp and 16#40#));

         if ((Val and 16#0F#) + (Val and 16#01#)) > 16#05# then
            Val := (Val and 16#F0#) or ((Val + 16#06#) and 16#0F#);
         end if;

         if (Unsigned_16 (Val and 16#F0#) + Unsigned_16 (Val and 16#10#)) >
           16#50#
         then
            Val    := (Val and 16#0F#) or ((Val + 16#60#) and 16#F0#);
            Flag_C := True;
         else
            Flag_C := False;
         end if;
      else
         Set_NZ (Val, Flag_N, Flag_Z);
         Flag_C := (Val and 16#40#) /= 0;
         Flag_V := ((Val and 16#40#) xor Shift_Left (Val and 16#20#, 1)) /= 0;
      end if;

      A := Val;
   end Op_ARR;

   procedure Op_ASL
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Flag_C, Flag_N, Flag_Z : out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_C := (Val and 16#80#) /= 0;
      Val    := Shift_Left (Val and 16#7F#, 1);
      Set_NZ (Val, Flag_N, Flag_Z);
      Mem_IO_Write (C, Mem, Address, Val);
   end Op_ASL;

   procedure Op_ASLA
     (A : in out Unsigned_8; Flag_C, Flag_N, Flag_Z : out Boolean)
   is
   begin
      Flag_C := (A and 16#80#) /= 0;
      A      := Shift_Left (A and 16#7F#, 1);
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_ASLA;

   procedure Op_ASO
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : in out Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_C := (Val and 16#80#) /= 0;
      Val    := Shift_Left (Val and 16#7F#, 1);
      Mem_IO_Write (C, Mem, Address, Val);
      A := A or Val;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_ASO;

   procedure Op_AXA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A, X, Base_High_Plus_1 : Unsigned_8)
   is
      Val : constant Unsigned_8 := A and X and Base_High_Plus_1;
   begin
      Mem_IO_Write (C, Mem, Address, Val);
   end Op_AXA;

   procedure Op_AXS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A, X : Unsigned_8)
   is
      Val : constant Unsigned_8 := A and X;
   begin
      Mem_IO_Write (C, Mem, Address, Val);
   end Op_AXS;

   --  procedure Op_BBR
   --    (C  : in out CPU_6502_Series'Class; Mem : not null access
   --     RAM_All_Banks; PC : in out Unsigned_16; Test_Bit : Unsigned_8)
   --  is
   --     Address       : Unsigned_16;
   --     Distance, Val : Unsigned_8;
   --  begin
   --     Mode_ZPG (C, Mem, PC, Address);
   --     Mode_REL (C, Mem, PC, Distance);
   --     Mem_IO_Read (C, Mem, Address, Val);
   --     if (Val and Test_Bit) = 0 then
   --        Branch_Taken (PC, Distance);
   --     end if;
   --  end Op_BBR;

   --  procedure Op_BBS
   --    (C  : in out CPU_6502_Series'Class; Mem : not null access
   --     RAM_All_Banks; PC : in out Unsigned_16; Test_Bit : Unsigned_8)
   --  is
   --     Address       : Unsigned_16;
   --     Distance, Val : Unsigned_8;
   --  begin
   --     Mode_ZPG (C, Mem, PC, Address);
   --     Mode_REL (C, Mem, PC, Distance);
   --     Mem_IO_Read (C, Mem, Address, Val);
   --     if (Val and Test_Bit) /= 0 then
   --        Branch_Taken (PC, Distance);
   --     end if;
   --  end Op_BBS;

   procedure Op_BCC
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_C : Boolean;
      Extra_Cycles :    out Unsigned_8)
   is
   begin
      if not Flag_C then
         Branch_Taken (PC, Distance, Extra_Cycles);
      else
         Extra_Cycles := 0;
      end if;
   end Op_BCC;

   procedure Op_BCS
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_C : Boolean;
      Extra_Cycles :    out Unsigned_8)
   is
   begin
      if Flag_C then
         Branch_Taken (PC, Distance, Extra_Cycles);
      else
         Extra_Cycles := 0;
      end if;
   end Op_BCS;

   procedure Op_BEQ
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_Z : Boolean;
      Extra_Cycles :    out Unsigned_8)
   is
   begin
      if Flag_Z then
         Branch_Taken (PC, Distance, Extra_Cycles);
      else
         Extra_Cycles := 0;
      end if;
   end Op_BEQ;

   procedure Op_BIT
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : Unsigned_8;
      Flag_N, Flag_V, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_Z := (A and Val) = 0;
      Flag_N := (Val and 16#80#) /= 0;
      Flag_V := (Val and 16#40#) /= 0;
   end Op_BIT;

   procedure Op_BITI
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A : Unsigned_8; Flag_Z : out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_Z := (A and Val) = 0;
   end Op_BITI;

   procedure Op_BMI
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_N : Boolean;
      Extra_Cycles :    out Unsigned_8)
   is
   begin
      if Flag_N then
         Branch_Taken (PC, Distance, Extra_Cycles);
      else
         Extra_Cycles := 0;
      end if;
   end Op_BMI;

   procedure Op_BNE
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_Z : Boolean;
      Extra_Cycles :    out Unsigned_8)
   is
   begin
      if not Flag_Z then
         Branch_Taken (PC, Distance, Extra_Cycles);
      else
         Extra_Cycles := 0;
      end if;
   end Op_BNE;

   procedure Op_BPL
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_N : Boolean;
      Extra_Cycles :    out Unsigned_8)
   is
   begin
      if not Flag_N then
         Branch_Taken (PC, Distance, Extra_Cycles);
      else
         Extra_Cycles := 0;
      end if;
   end Op_BPL;

   procedure Op_BRA
     (PC           : in out Unsigned_16; Distance : Unsigned_8;
      Extra_Cycles :    out Unsigned_8)
   is
   begin
      Branch_Taken (PC, Distance, Extra_Cycles);
   end Op_BRA;

   procedure Op_BRK
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      P : in out CPU_6502_P; PC : in out Unsigned_16; SP : in out Unsigned_8)
   is
   begin
      PC := PC + 1;
      Push (C, Mem, SP, Unsigned_8 (Shift_Right (PC, 8)));
      Push (C, Mem, SP, Unsigned_8 (PC and 16#FF#));
      Push (C, Mem, SP, Unsigned_8 (P));
      P := P or P_Flag_IRQB_Disable;
      Get_Word (C, Mem, 16#FFFE#, PC);
   end Op_BRK;

   procedure Op_BVC
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_V : Boolean;
      Extra_Cycles :    out Unsigned_8)
   is
   begin
      if not Flag_V then
         Branch_Taken (PC, Distance, Extra_Cycles);
      else
         Extra_Cycles := 0;
      end if;
   end Op_BVC;

   procedure Op_BVS
     (PC : in out Unsigned_16; Distance : Unsigned_8; Flag_V : Boolean;
      Extra_Cycles :    out Unsigned_8)
   is
   begin
      if Flag_V then
         Branch_Taken (PC, Distance, Extra_Cycles);
      else
         Extra_Cycles := 0;
      end if;
   end Op_BVS;

   procedure Op_CLC (Flag_C : out Boolean) is
   begin
      Flag_C := False;
   end Op_CLC;

   procedure Op_CLD (Flag_BCD : out Boolean) is
   begin
      Flag_BCD := False;
   end Op_CLD;

   procedure Op_CLI (P : in out CPU_6502_P) is
   begin
      P := P and not P_Flag_IRQB_Disable;
   end Op_CLI;

   procedure Op_CLV (Flag_V : out Boolean) is
   begin
      Flag_V := False;
   end Op_CLV;

   procedure Op_CMP
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_C := (A >= Val);
      Val    := A - Val;
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_CMP;

   procedure Op_CPX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; X : Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_C := (X >= Val);
      Val    := X - Val;
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_CPX;

   procedure Op_CPY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; Y : Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_C := (Y >= Val);
      Val    := Y - Val;
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_CPY;

   procedure Op_DCM
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Val := Val - 1;
      Mem_IO_Write (C, Mem, Address, Val);
      Flag_C := (A >= Val);
      Val    := A - Val;
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_DCM;

   procedure Op_DEA (A : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) is
   begin
      A := A - 1;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_DEA;

   procedure Op_DEC
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Flag_N, Flag_Z : out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Val := Val - 1;
      Mem_IO_Write (C, Mem, Address, Val);
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_DEC;

   procedure Op_DEX (X : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) is
   begin
      X := X - 1;
      Set_NZ (X, Flag_N, Flag_Z);
   end Op_DEX;

   procedure Op_DEY (Y : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) is
   begin
      Y := Y - 1;
      Set_NZ (Y, Flag_N, Flag_Z);
   end Op_DEY;

   procedure Op_EOR
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; A : in out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      A := A xor Val;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_EOR;

   procedure Op_HLT
     (C      : in out CPU_6502_Series'Class; PC : in out Unsigned_16;
      Opcode :        Unsigned_8)
   is
   begin
      PC            := PC - 1;
      C.Halt_Opcode := Opcode;
   end Op_HLT;

   procedure Op_INA (A : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) is
   begin
      A := A + 1;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_INA;

   procedure Op_INC
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Flag_N, Flag_Z : out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Val := Val + 1;
      Mem_IO_Write (C, Mem, Address, Val);
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_INC;

   procedure Op_INS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean)
   is
      Temp       : Unsigned_8;
      Val, Temp2 : Unsigned_16;
   begin
      Mem_IO_Read (C, Mem, Address, Temp);
      Temp := Temp + 1;
      Mem_IO_Write (C, Mem, Address, Temp);

      Temp2 :=
        Unsigned_16 (A) - Unsigned_16 (Temp) - (if Flag_C then 0 else 1);

      if Flag_Decimal then
         Val :=
           Unsigned_16 (A and 16#0F#) - Unsigned_16 (Temp and 16#0F#) -
           (if Flag_C then 0 else 1);

         if (Val and 16#10#) /= 0 then
            Val :=
              ((Val - 16#06#) and 16#0F#) or
              (Unsigned_16 (A and 16#F0#) - Unsigned_16 (Temp and 16#F0#) -
               16#10#);
         else
            Val :=
              (Val and 16#0F#) or
              (Unsigned_16 (A and 16#F0#) - Unsigned_16 (Temp and 16#F0#));
         end if;

         if (Val and 16#0100#) /= 0 then
            Val := Val - 16#60#;
         end if;

         Flag_C := (Temp2 < 16#0100#);
         Set_NZ (Unsigned_8 (Temp2 and 16#FF#), Flag_N, Flag_Z);

         Flag_V :=
           (A and 16#80#) /= (Temp and 16#80#) and
           (A and 16#80#) /= Unsigned_8 (Temp2 and 16#80#);
      else
         Val    := Temp2;
         Flag_C := (Val < 16#0100#);

         Flag_V :=
           (A and 16#80#) /= (Temp and 16#80#) and
           (A and 16#80#) /= Unsigned_8 (Val and 16#80#);

         Set_NZ (Unsigned_8 (Val and 16#FF#), Flag_N, Flag_Z);
      end if;

      A := Unsigned_8 (Val and 16#FF#);
   end Op_INS;

   procedure Op_INX (X : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) is
   begin
      X := X + 1;
      Set_NZ (X, Flag_N, Flag_Z);
   end Op_INX;

   procedure Op_INY (Y : in out Unsigned_8; Flag_N, Flag_Z : out Boolean) is
   begin
      Y := Y + 1;
      Set_NZ (Y, Flag_N, Flag_Z);
   end Op_INY;

   procedure Op_JMP (PC : out Unsigned_16; Address : Unsigned_16) is
   begin
      PC := Address;
   end Op_JMP;

   procedure Op_JSR
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      PC : in out Unsigned_16; SP : in out Unsigned_8; Address : Unsigned_16)
   is
   begin
      PC := PC - 1;
      Push (C, Mem, SP, Unsigned_8 (Shift_Right (PC, 8)));
      Push (C, Mem, SP, Unsigned_8 (PC and 16#FF#));
      PC := Address;
   end Op_JSR;

   procedure Op_LAS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; SP : in out Unsigned_8; A, X : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Val := Val and SP;
      A   := Val;
      X   := Val;
      SP  := Val;
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_LAS;

   procedure Op_LAX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; A, X : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      A := Val;
      X := Val;
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_LAX;

   procedure Op_LDA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address : Unsigned_16; A : out Unsigned_8; Flag_N, Flag_Z : out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      A := Val;
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_LDA;

   procedure Op_LDX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address : Unsigned_16; X : out Unsigned_8; Flag_N, Flag_Z : out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      X := Val;
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_LDX;

   procedure Op_LDY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address : Unsigned_16; Y : out Unsigned_8; Flag_N, Flag_Z : out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Y := Val;
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_LDY;

   procedure Op_LSE
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address                :        Unsigned_16; A : in out Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_C := (Val and 16#01#) /= 0;
      Val    := Shift_Right (Val, 1);
      Mem_IO_Write (C, Mem, Address, Val);
      A := A xor Val;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_LSE;

   procedure Op_LSR
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Flag_C, Flag_N, Flag_Z : out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_C := (Val and 16#01#) /= 0;
      Val    := Shift_Right (Val, 1);
      Mem_IO_Write (C, Mem, Address, Val);
      Flag_N := False;
      Set_Z (Val, Flag_Z);
   end Op_LSR;

   procedure Op_LSRA
     (A : in out Unsigned_8; Flag_C, Flag_N, Flag_Z : out Boolean)
   is
   begin
      Flag_C := (A and 16#01#) /= 0;
      A      := Shift_Right (A, 1);
      Flag_N := False;
      Set_Z (A, Flag_Z);
   end Op_LSRA;

   procedure Op_OAL
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        : Unsigned_16; A : in out Unsigned_8; X : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      A := (A or 16#EE#) and Val;
      X := A;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_OAL;

   procedure Op_ORA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; A : in out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      A := A or Val;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_ORA;

   procedure Op_PHA
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; A : Unsigned_8)
   is
   begin
      Push (C, Mem, SP, A);
   end Op_PHA;

   procedure Op_PHP
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; P : CPU_6502_P)
   is
   begin
      Push (C, Mem, SP, Unsigned_8 (P));
   end Op_PHP;

   procedure Op_PHX
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; X : Unsigned_8)
   is
   begin
      Push (C, Mem, SP, X);
   end Op_PHX;

   procedure Op_PHY
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; Y : Unsigned_8)
   is
   begin
      Push (C, Mem, SP, Y);
   end Op_PHY;

   procedure Op_PLA
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; A : out Unsigned_8; Flag_N, Flag_Z : out Boolean)
   is
   begin
      Pop (C, Mem, SP, A);
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_PLA;

   procedure Op_PLP
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; P : out CPU_6502_P)
   is
      Val : Unsigned_8;
   begin
      Pop (C, Mem, SP, Val);
      P := CPU_6502_P (Val) or P_Flag_BRK_Command;
   end Op_PLP;

   procedure Op_PLX
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; X : out Unsigned_8; Flag_N, Flag_Z : out Boolean)
   is
   begin
      Pop (C, Mem, SP, X);
      Set_NZ (X, Flag_N, Flag_Z);
   end Op_PLX;

   procedure Op_PLY
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; Y : out Unsigned_8; Flag_N, Flag_Z : out Boolean)
   is
   begin
      Pop (C, Mem, SP, Y);
      Set_NZ (Y, Flag_N, Flag_Z);
   end Op_PLY;

   procedure Op_RLA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_Z :    out Boolean)
   is
      Val        : Unsigned_8;
      Tmp_Flag_C : Boolean;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Tmp_Flag_C := (Val and 16#80#) /= 0;
      Val := Shift_Left (Val and 16#7F#, 1) or (if Flag_C then 1 else 0);
      Flag_C     := Tmp_Flag_C;
      Mem_IO_Write (C, Mem, Address, Val);
      A := A and Val;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_RLA;

   --  procedure Op_RMB
   --    (C : in out CPU_6502_Series'Class; Mem : not null access
   --     RAM_All_Banks; Address : Unsigned_16; Reset_Bit : Unsigned_8)
   --  is
   --     Val : Unsigned_8;
   --  begin
   --     Mem_IO_Read (C, Mem, Address, Val);
   --     Val := Val and not Reset_Bit;
   --     Mem_IO_Write (C, Mem, Address, Val);
   --  end Op_RMB;

   procedure Op_ROL
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; Flag_C : in out Boolean;
      Flag_N, Flag_Z :    out Boolean)
   is
      Val        : Unsigned_8;
      Tmp_Flag_C : Boolean;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Tmp_Flag_C := (Val and 16#80#) /= 0;
      Val := Shift_Left (Val and 16#7F#, 1) or (if Flag_C then 1 else 0);
      Flag_C     := Tmp_Flag_C;
      Mem_IO_Write (C, Mem, Address, Val);
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_ROL;

   procedure Op_ROLA
     (A              : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_Z :    out Boolean)
   is
      Tmp_Flag_C : Boolean;
   begin
      Tmp_Flag_C := (A and 16#80#) /= 0;
      A          := Shift_Left (A and 16#7F#, 1) or (if Flag_C then 1 else 0);
      Flag_C     := Tmp_Flag_C;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_ROLA;

   procedure Op_ROR
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; Flag_C : in out Boolean;
      Flag_N, Flag_Z :    out Boolean)
   is
      Val        : Unsigned_8;
      Tmp_Flag_C : Boolean;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Tmp_Flag_C := (Val and 16#01#) /= 0;
      Val        := Shift_Right (Val, 1) or (if Flag_C then 16#80# else 0);
      Flag_C     := Tmp_Flag_C;
      Mem_IO_Write (C, Mem, Address, Val);
      Set_NZ (Val, Flag_N, Flag_Z);
   end Op_ROR;

   procedure Op_RORA
     (A              : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_Z :    out Boolean)
   is
      Tmp_Flag_C : Boolean;
   begin
      Tmp_Flag_C := (A and 16#01#) /= 0;
      A          := Shift_Right (A, 1) or (if Flag_C then 16#80# else 0);
      Flag_C     := Tmp_Flag_C;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_RORA;

   procedure Op_RRA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean)
   is
      Temp       : Unsigned_8;
      Val        : Unsigned_16;
      Tmp_Flag_C : Boolean;
   begin
      Mem_IO_Read (C, Mem, Address, Temp);
      Tmp_Flag_C := (Temp and 16#01#) /= 0;
      Temp       := Shift_Right (Temp, 1) or (if Flag_C then 16#80# else 0);
      Flag_C     := Tmp_Flag_C;
      Mem_IO_Write (C, Mem, Address, Temp);

      if Flag_Decimal then
         Val :=
           Unsigned_16 (A and 16#0F#) + Unsigned_16 (Temp and 16#0F#) +
           (if Flag_C then 1 else 0);

         if Val > 16#09# then
            Val := Val + 16#06#;
         end if;

         Flag_C := (Val > 16#0F#);  --  carry flag for low digit

         Val :=
           (Val and 16#0F#) + Unsigned_16 (A and 16#F0#) +
           Unsigned_16 (Temp and 16#F0#) + (if Flag_C then 16#10# else 0);

         Flag_Z := (A + Temp + (if Tmp_Flag_C then 1 else 0)) = 0;
         Flag_N := (Val and 16#80#) /= 0;

         Flag_V :=
           ((A and 16#80#) = (Temp and 16#80#)) and
           ((A and 16#80#) /= Unsigned_8 (Val and 16#80#));

         if (Val and 16#01F0#) > 16#90# then
            Val := Val + 16#60#;
         end if;

         Flag_C := (Val and 16#0FF0#) > 16#F0#;
         A      := Unsigned_8 (Val and 16#FF#);
      else
         Val :=
           Unsigned_16 (A) + Unsigned_16 (Temp) + (if Flag_C then 1 else 0);

         Flag_C := Val > 16#FF#;

         Flag_V :=
           ((A and 16#80#) = (Temp and 16#80#)) and
           ((A and 16#80#) /= Unsigned_8 (Val and 16#80#));

         A := Unsigned_8 (Val and 16#FF#);
         Set_NZ (A, Flag_N, Flag_Z);
      end if;
   end Op_RRA;

   procedure Op_RTI
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; P : out CPU_6502_P; PC : out Unsigned_16)
   is
      Val, Val2 : Unsigned_8;
   begin
      Pop (C, Mem, SP, Val);
      P := CPU_6502_P (Val) or P_Flag_BRK_Command;
      Pop (C, Mem, SP, Val);
      Pop (C, Mem, SP, Val2);
      PC := Unsigned_16 (Val) or Shift_Left (Unsigned_16 (Val2), 8);
   end Op_RTI;

   procedure Op_RTS
     (C  : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      SP : in out Unsigned_8; PC : out Unsigned_16)
   is
      Val, Val2 : Unsigned_8;
   begin
      Pop (C, Mem, SP, Val);
      Pop (C, Mem, SP, Val2);
      PC := Unsigned_16 (Val) or Shift_Left (Unsigned_16 (Val2), 8);
      PC := PC + 1;
   end Op_RTS;

   procedure Op_SAX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A : Unsigned_8; X : in out Unsigned_8;
      Flag_C, Flag_N, Flag_Z :    out Boolean)
   is
      Temp : constant Unsigned_8 := A and X;
      Val  : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_C := (Temp >= Val);
      X      := Temp - Val;
      Set_NZ (X, Flag_N, Flag_Z);
   end Op_SAX;

   procedure Op_SAY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Y, Base_High_Plus_1 : Unsigned_8)
   is
      Val : constant Unsigned_8 := Y and Base_High_Plus_1;
   begin
      Mem_IO_Write (C, Mem, Address, Val);
   end Op_SAY;

   procedure Op_SBC_NMOS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean)
   is
      Temp       : Unsigned_8;
      Temp2, Val : Unsigned_16;
   begin
      Mem_IO_Read (C, Mem, Address, Temp);

      Temp2 :=
        Unsigned_16 (A) - Unsigned_16 (Temp) - (if Flag_C then 0 else 1);

      if Flag_Decimal then
         Val :=
           Unsigned_16 (A and 16#0F#) - Unsigned_16 (Temp and 16#0F#) -
           (if Flag_C then 0 else 1);

         if (Val and 16#10#) /= 0 then
            Val :=
              ((Val - 16#06#) and 16#0F#) or
              (Unsigned_16 (A and 16#F0#) - Unsigned_16 (Temp and 16#F0#) -
               16#10#);
         else
            Val :=
              (Val and 16#0F#) or
              (Unsigned_16 (A and 16#F0#) - Unsigned_16 (Temp and 16#F0#));
         end if;

         if (Val and 16#0100#) /= 0 then
            Val := Val - 16#60#;
         end if;

         Flag_C := (Temp2 < 16#0100#);
         Set_NZ (Unsigned_8 (Temp2 and 16#FF#), Flag_N, Flag_Z);

         Flag_V :=
           (A and 16#80#) /= (Temp and 16#80#) and
           (A and 16#80#) /= Unsigned_8 (Temp2 and 16#80#);

         A := Unsigned_8 (Val and 16#FF#);
      else
         Val    := Temp2;
         Flag_C := (Val < 16#0100#);

         Flag_V :=
           (A and 16#80#) /= (Temp and 16#80#) and
           (A and 16#80#) /= Unsigned_8 (Val and 16#80#);

         A := Unsigned_8 (Val and 16#FF#);
         Set_NZ (A, Flag_N, Flag_Z);
      end if;
   end Op_SBC_NMOS;

   procedure Op_SBC_CMOS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :    Unsigned_16; A : in out Unsigned_8; Flag_C : in out Boolean;
      Flag_N, Flag_V, Flag_Z :    out Boolean; Flag_Decimal : Boolean;
      Extra_Cycles           : in out Unsigned_8)
   is
      Temp       : Unsigned_8;
      Temp2, Val : Unsigned_16;
   begin
      Mem_IO_Read (C, Mem, Address, Temp);

      Flag_V := (A and 16#80#) /= (Temp and 16#80#);

      if Flag_Decimal then
         Extra_Cycles := Extra_Cycles + 1;

         Temp2 :=
           16#0F# + Unsigned_16 (A and 16#0F#) -
           Unsigned_16 (Temp and 16#0F#) + (if Flag_C then 1 else 0);

         if Temp2 < 16#10# then
            Val   := 0;
            Temp2 := Temp2 - 16#06#;
         else
            Val   := 16#10#;
            Temp2 := Temp2 - 16#10#;
         end if;

         Val :=
           Val + 16#F0# + Unsigned_16 (A and 16#F0#) -
           Unsigned_16 (Temp and 16#F0#);

         if Val < 16#0100# then
            Flag_C := False;
            if Val < 16#80# then
               Flag_V := False;
            end if;
            Val := Val - 16#60#;
         else
            Flag_C := True;
            if Val >= 16#0180# then
               Flag_V := False;
            end if;
            Val := Val + Temp2;
         end if;
      else
         Val :=
           16#FF# + Unsigned_16 (A) - Unsigned_16 (Temp) +
           (if Flag_C then 1 else 0);

         if Val < 16#0100# then
            Flag_C := False;
            if Val < 16#80# then
               Flag_V := False;
            end if;
         else
            Flag_C := True;
            if Val >= 16#0180# then
               Flag_V := False;
            end if;
         end if;
      end if;

      A := Unsigned_8 (Val and 16#FF#);
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_SBC_CMOS;

   procedure Op_SEC (Flag_C : out Boolean) is
   begin
      Flag_C := True;
   end Op_SEC;

   procedure Op_SED (Flag_BCD : out Boolean) is
   begin
      Flag_BCD := True;
   end Op_SED;

   procedure Op_SEI (P : in out CPU_6502_P) is
   begin
      P := P or P_Flag_IRQB_Disable;
   end Op_SEI;

   --  procedure Op_SMB
   --    (C : in out CPU_6502_Series'Class; Mem : not null access
   --     RAM_All_Banks; Address : Unsigned_16; Set_Bit : Unsigned_8)
   --  is
   --     Val : Unsigned_8;
   --  begin
   --     Mem_IO_Read (C, Mem, Address, Val);
   --     Val := Val or Set_Bit;
   --     Mem_IO_Write (C, Mem, Address, Val);
   --  end Op_SMB;

   procedure Op_STA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A : Unsigned_8)
   is
   begin
      Mem_IO_Write (C, Mem, Address, A);
   end Op_STA;

   procedure Op_STX
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; X : Unsigned_8)
   is
   begin
      Mem_IO_Write (C, Mem, Address, X);
   end Op_STX;

   procedure Op_STY
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; Y : Unsigned_8)
   is
   begin
      Mem_IO_Write (C, Mem, Address, Y);
   end Op_STY;

   procedure Op_STZ
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16)
   is
   begin
      Mem_IO_Write (C, Mem, Address, 0);
   end Op_STZ;

   procedure Op_TAS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A, X, Base_High_Plus_1 : Unsigned_8;
      SP      :    out Unsigned_8)
   is
      Val : Unsigned_8 := A and X;
   begin
      SP  := Val;
      Val := Val and Base_High_Plus_1;
      Mem_IO_Write (C, Mem, Address, Val);
   end Op_TAS;

   procedure Op_TAX
     (A : Unsigned_8; X : out Unsigned_8; Flag_N, Flag_Z : out Boolean)
   is
   begin
      X := A;
      Set_NZ (X, Flag_N, Flag_Z);
   end Op_TAX;

   procedure Op_TAY
     (A : Unsigned_8; Y : out Unsigned_8; Flag_N, Flag_Z : out Boolean)
   is
   begin
      Y := A;
      Set_NZ (Y, Flag_N, Flag_Z);
   end Op_TAY;

   procedure Op_TRB
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A : Unsigned_8; Flag_Z : out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_Z := ((A and Val) = 0);
      Val    := Val and not A;
      Mem_IO_Write (C, Mem, Address, Val);
   end Op_TRB;

   procedure Op_TSB
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; A : Unsigned_8; Flag_Z : out Boolean)
   is
      Val : Unsigned_8;
   begin
      Mem_IO_Read (C, Mem, Address, Val);
      Flag_Z := ((A and Val) = 0);
      Val    := Val or A;
      Mem_IO_Write (C, Mem, Address, Val);
   end Op_TSB;

   procedure Op_TSX
     (SP : Unsigned_8; X : out Unsigned_8; Flag_N, Flag_Z : out Boolean)
   is
   begin
      X := SP;
      Set_NZ (X, Flag_N, Flag_Z);
   end Op_TSX;

   procedure Op_TXA
     (X : Unsigned_8; A : out Unsigned_8; Flag_N, Flag_Z : out Boolean)
   is
   begin
      A := X;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_TXA;

   procedure Op_TXS (X : Unsigned_8; SP : out Unsigned_8) is
   begin
      SP := X;
   end Op_TXS;

   procedure Op_TYA
     (Y : Unsigned_8; A : out Unsigned_8; Flag_N, Flag_Z : out Boolean)
   is
   begin
      A := Y;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_TYA;

   procedure Op_XAA
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address        :        Unsigned_16; X : Unsigned_8; A : out Unsigned_8;
      Flag_N, Flag_Z :    out Boolean)
   is
      Val : Unsigned_8;
   begin
      A := X;
      Mem_IO_Read (C, Mem, Address, Val);
      A := A and Val;
      Set_NZ (A, Flag_N, Flag_Z);
   end Op_XAA;

   procedure Op_XAS
     (C : in out CPU_6502_Series'Class; Mem : not null access RAM_All_Banks;
      Address :        Unsigned_16; X, Base_High_Plus_1 : Unsigned_8)
   is
      Val : constant Unsigned_8 := X and Base_High_Plus_1;
   begin
      Mem_IO_Write (C, Mem, Address, Val);
   end Op_XAS;

end MOS_CPU_6502.Shared;
