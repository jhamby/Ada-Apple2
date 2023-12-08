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

with Emu; use Emu;

with Emu.Memory; use Emu.Memory;

with Interfaces; use Interfaces;

with MOS_CPU_6502; use MOS_CPU_6502;

package Apple2 with
  SPARK_Mode
is

   -----------------------------------------
   -- Apple II/II+/IIe/etc. variant types --
   -----------------------------------------

   type Apple_2_Model is (Apple_2, Apple_2_Plus, Apple_2e, Apple_2e_Enhanced);
   --  A2 model: Apple II, II Plus, IIe, or IIe Enhanced (WDC 65C02)

   type Video_Standard_Type is (NTSC, PAL);
   --  Use NTSC or PAL timings for video

   ----------------------
   -- System ROM sizes --
   ----------------------

   Cx_ROM_Size : constant := 4 * 1_024; --  4K Cx ROM size

   Apple_2_ROM_Size : constant := 12 * 1_024; --  12K ROM (II / II Plus)

   Apple_2e_ROM_Size : constant := Apple_2_ROM_Size + Cx_ROM_Size;
   --  16K ROM (IIe / IIe Enhanced)

   ------------------------------------------------------
   -- Clock speeds / durations for NTSC & PAL machines --
   ------------------------------------------------------

   NTSC_M14 : constant Long_Float := 157_500_000.0 / 11.0;
   --  14.3181818... * 10^6

   NTSC_Clock_6502 : constant Long_Float := (NTSC_M14 * 65.0) / 912.0;
   --  65 cycles per 912 14M clocks

   NTSC_Clock_Z80 : constant Long_Float := NTSC_Clock_6502 * 2.0;
   --  effective Z80 clock rate is 2.041 MHz

   Cycles_Per_Line : constant := 65;
   --  25 cycles of HBL & 40 cycles of HBL

   NTSC_Lines_Per_Frame : constant := 262;
   --  64 in each third of the screen & 70 in VBL

   NTSC_Clocks_Per_Frame : constant := Cycles_Per_Line * NTSC_Lines_Per_Frame;
   --  17030

   PAL_Lines_Per_Frame : constant := 312;
   --  64 in each third of the screen & 120 in VBL

   PAL_Clocks_Per_Frame : constant := Cycles_Per_Line * PAL_Lines_Per_Frame;
   --  17030

   -----------------------------------------
   -- Emulator app state and default keys --
   -----------------------------------------

   --  Use a base audio freq so the OS is less likely to need to resample.
   --  Assume base freqs are 44.1 kHz & 48 kHz.

   Max_Sample_Rate : constant := 48_000;
   --  Try 48 kHz sample rate first

   Fallback_Sample_Rate : constant := 44_100;
   --  Try 44.1 kHz as the fallback sample rate (or else no audio)

   type App_Mode is
     (Logo,
      Paused,
      Running,    --  6502 running at normal speed (breakpoints may be active)
      Debug,      --  6502 paused
      Stepping);  --  6502 running at full speed (breakpoints always active)

   Speed_Min    : constant := 1;   --  0.1 MHz min speed
   Speed_Normal : constant := 10;  --  1.0 MHz base speed
   Speed_Max    : constant := 40;  --  4.0 MHz max speed

   type Draw_Config_Type is mod 256;

   Draw_Background    : constant Draw_Config_Type := 16#01#;
   Draw_LEDs          : constant Draw_Config_Type := 16#02#;
   Draw_Title         : constant Draw_Config_Type := 16#04#;
   Draw_Button_Drives : constant Draw_Config_Type := 16#08#;

   type Func_Key_Type is range 0 .. 11;
   --  Function Keys F1 - F12

   Func_Key_Help        : constant Func_Key_Type := 0;
   Func_Key_Run         : constant Func_Key_Type := 1;
   Func_Key_Drive1      : constant Func_Key_Type := 2;
   Func_Key_Drive2      : constant Func_Key_Type := 3;
   Func_Key_Drive_Swap  : constant Func_Key_Type := 4;
   Func_Key_Full_Screen : constant Func_Key_Type := 5;
   Func_Key_Debug       : constant Func_Key_Type := 6;
   Func_Key_Setup       : constant Func_Key_Type := 7;
   Func_Key_Cycle       : constant Func_Key_Type := 8;
   Func_Key_Quit        : constant Func_Key_Type := 11;
   Func_Key_Save_State  : constant Func_Key_Type := 10;
   Func_Key_Load_State  : constant Func_Key_Type := 9;

   ----------------------------------
   -- Apple II series system state --
   ----------------------------------

   Num_Slots : constant := 8;

   type Slot_Range is range 0 .. Num_Slots - 1;

   type Expansion_ROM_Type is (ROM_None, ROM_Internal, ROM_Peripheral);
   --  Select the enabled ROM at $C100..$CFFF

   type Mem_Flag_Type is mod 2**16;
   --  Memory map behavior flags

   Flag_80_Store        : constant Mem_Flag_Type := 16#0001#;
   Flag_Alt_ZP          : constant Mem_Flag_Type := 16#0002#;
   Flag_Aux_Read        : constant Mem_Flag_Type := 16#0004#;
   Flag_Aux_Write       : constant Mem_Flag_Type := 16#0008#;
   Flag_High_RAM_Bank_2 : constant Mem_Flag_Type := 16#0010#;
   Flag_High_RAM        : constant Mem_Flag_Type := 16#0020#;
   Flag_Hi_Res          : constant Mem_Flag_Type := 16#0040#;
   Flag_Page_2          : constant Mem_Flag_Type := 16#0080#;
   Flag_Slot_C3_ROM     : constant Mem_Flag_Type := 16#0100#;
   Flag_Slot_CX_ROM     : constant Mem_Flag_Type := 16#0200#;
   Flag_High_RAM_Write  : constant Mem_Flag_Type := 16#0400#;

   type Mem_Init_Pattern_Type is (Pattern_Zero, Pattern_FF_FF_00_00);

   type IRQ_Source is (IRQ_6522, IRQ_Speech, IRQ_SSC, IRQ_MOUSE);

   type Button_ID is (Button_0, Button_1);
   type Button_State is (Button_Up, Button_Down);

   type Video_Flag_Type is mod 2**16;

   Vid_Flag_80_Column : constant Video_Flag_Type := 16#0001#;
   Vid_Flag_Dbl_Hires : constant Video_Flag_Type := 16#0002#;
   Vid_Flag_Hires     : constant Video_Flag_Type := 16#0004#;
   Vid_Flag_Mask_2    : constant Video_Flag_Type := 16#0008#;
   Vid_Flag_Mixed     : constant Video_Flag_Type := 16#0010#;
   Vid_Flag_Page_2    : constant Video_Flag_Type := 16#0020#;
   Vid_Flag_Text      : constant Video_Flag_Type := 16#0040#;

   type Mem_Page_Table is array (Unsigned_8) of RAM_Bank_Index;
   --  Table of RAM bank to use within RAM_Pages for each page in 64K

   type Page_Flag_Type is mod 2**8;
   --  Flags for each page, cleared when written to by any source

   Page_Flag_Reset : constant Page_Flag_Type := 16#00#;
   --  clear all flags on any page write

   Page_Flag_Video : constant Page_Flag_Type := 16#01#;
   --  set when screen update uses this page for the image

   type Page_Clean_Table is array (Unsigned_8) of Page_Flag_Type;
   --  Table of one-bit usage flags for each 256-byte page in 64 KB
   --  Video uses this to detect when screen memory changes

   -------------------------------------
   -- Apple II/II+/IIe Computer State --
   -------------------------------------

   type Apple2_Base is abstract new CPU_6502_Series with record

      Model : Apple_2_Model := Apple_2e_Enhanced;

      Mem_Mode : Mem_Flag_Type :=
        Flag_High_RAM_Bank_2 or Flag_Slot_CX_ROM or Flag_High_RAM_Write;
      --  Memory mode flags (initialized to startup value)

      Mem_Init_Pattern : Mem_Init_Pattern_Type := Pattern_FF_FF_00_00;
      --  Current memory init pattern

      Mem_Read_Bank : Mem_Page_Table := (others => RAM_Bank_Main);
      --  Memory read paging table (no bank specified)

      Mem_Write_Bank : Mem_Page_Table := (others => RAM_Bank_Main);
      --  Memory write paging table

      Page_Clean_Flags : Page_Clean_Table := (others => Page_Flag_Reset);
      --  page written flags (used by Video)

      RAM_Active_Bank : RAM_Bank_Index := RAM_Bank_Aux;
      --  active aux RAM bank

      RAM_Max_Bank_Used : RAM_Bank_Index := RAM_Bank_Aux;
      --  highest RAMWorks III bank read or written to

      IO_Select_Slot : Slot_Range := 0;
      --  I/O select peripheral ROM to enable (1 - 7 = slots)

      IO_Select_Internal_ROM : Boolean := False;
      --  Enable internal Apple IIe ROM at $C100 .. $CFFF

      Expansion_ROM : Expansion_ROM_Type := ROM_None;
      --  Select the enabled ROM at $C100 .. $CFFF

      Video_Mode : Video_Flag_Type := 0;
      --  Current video mode

      Video_Standard : Video_Standard_Type := NTSC;

   end record;
   --  Holds the state for an Apple II variant with devices, except RAM/ROM

   function Mem_Read
     (Mem     : not null access constant RAM_All_Banks; Bank : RAM_Bank_Index;
      Address : Unsigned_16) return Unsigned_8;
   --  Read a byte from memory by bank and 16-bit address

   procedure Mem_Write
     (C    : in out Apple2_Base; Mem : not null access RAM_All_Banks;
      Bank :        RAM_Bank_Index; Address : Unsigned_16; Value : Unsigned_8);
   --  Write a byte to memory by bank and 16-bit address

   function Is_Apple2 (C : Apple2_Base) return Boolean;
   --  Is this an original Apple II or II Plus?

   function Get_Apple_2_Model (C : Apple2_Base) return Apple_2_Model;
   --  Returns the current machine type

   procedure Set_Apple_2_Model
     (C : in out Apple2_Base; New_Type : Apple_2_Model);
   --  Change machine type from the default Apple IIe Enhanced

   procedure CPU_Execute
     (C            : in out Apple2_Base; Mem : not null access RAM_All_Banks;
      Total_Cycles :        Natural);
   --  Emulate the CPU for the specified number of cycles (6502 / 65C02)

   -----------------------------------------------------------
   --  TODO: put these hardcoded strings in a resource file --
   -----------------------------------------------------------

   Title_Apple_2           : constant String := "Apple ][ Emulator";
   Title_Apple_2_Plus      : constant String := "Apple ][+ Emulator";
   Title_Apple_2e          : constant String := "Apple --e Emulator";
   Title_Apple_2e_Enhanced : constant String := "Enhanced Apple --e Emulator";

   Title_Paused   : constant String := " Paused ";
   Title_Stepping : constant String := "Stepping";

   --  Configuration

   Reg_Value_Apple_2_Type        : constant String := "Apple2 Type";
   Reg_Value_Speaker_Volume      : constant String := "Speaker Volume";
   Reg_Value_MB_Volume           : constant String := "Mockingboard Volume";
   Reg_Value_Sound_Card_Type     : constant String := "Soundcard Type";
   Reg_Value_Keyboard_Type       : constant String := "Keyboard Type";
   Reg_Value_KB_Charset_Switch   : constant String := "Keyboard Rocker Switch";
   Reg_Value_Save_State_Filename : constant String := "Save State Filename";
   Reg_Value_Save_State_On_Exit  : constant String := "Save State On Exit";
   Reg_Value_HDD_Enabled         : constant String := "Harddisk Enable";
   Reg_Value_HDD_Image_1         : constant String := "Harddisk Image 1";
   Reg_Value_HDD_Image_2         : constant String := "Harddisk Image 2";
   Reg_Value_Disk_Image_1        : constant String := "Disk Image 1";
   Reg_Value_Disk_Image_2        : constant String := "Disk Image 2";
   Reg_Value_Clock_Slot          : constant String := "Clock Enable";

   Reg_Value_Parallel_Printer_Filename : constant String :=
     "Parallel Printer Filename";
   Reg_Value_Printer_Append : constant String := "Append to printer file";
   Reg_Value_Printer_Idle_Limit : constant String := "Printer idle limit";

   Reg_Value_PDL_X_Trim         : constant String := "PDL X-Trim";
   Reg_Value_PDL_Y_Trim         : constant String := "PDL Y-Trim";
   Reg_Value_Scroll_Lock_Toggle : constant String := "ScrollLock Toggle";
   Reg_Value_Mouse_In_Slot_4    : constant String := "Mouse in slot 4";

   --  Preferences

   Reg_Value_Pref_Start_Dir      : constant String := "Slot 6 Directory";
   Reg_Value_Pref_HDD_Start_Dir  : constant String := "HDV Starting Directory";
   Reg_Value_Pref_Save_State_Dir : constant String := "Save State Directory";

   Reg_Value_Show_LEDs : constant String := "Show Leds";

   --  sizes of status panel

   Status_Panel_Width  : constant := 100;
   Status_Panel_Height : constant := 48;

private
   --  Inline pragmas for procedures and functions

   pragma Inline (Mem_Read);
   pragma Inline (Mem_Write);
   pragma Inline (Is_Apple2);
   pragma Inline (Get_Apple_2_Model);
   pragma Inline (Set_Apple_2_Model);
   pragma Inline (CPU_Execute);

   --  TODO: Remove the following pragmas when video emulation is ported

   pragma Unreferenced (Vid_Flag_Dbl_Hires);
   pragma Unreferenced (Vid_Flag_Hires);
   pragma Unreferenced (Vid_Flag_Mask_2);
   pragma Unreferenced (Vid_Flag_Mixed);
   pragma Unreferenced (Vid_Flag_Page_2);
   pragma Unreferenced (Vid_Flag_Text);

end Apple2;
