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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Emu; use Emu;

with Emu.Memory; use Emu.Memory;

with Interfaces; use Interfaces;

with MOS_CPU_6502; use MOS_CPU_6502;

with SDL.Inputs.Joysticks;
with SDL.Video.Palettes; use SDL.Video.Palettes;

package Apple2 with
  SPARK_Mode
is

   -----------------------------------------
   -- Apple II/II+/IIe/etc. variant types --
   -----------------------------------------

   type Apple_2_Model is (Apple_2, Apple_2_Plus, Apple_2e, Apple_2e_Enhanced);
   --  Apple II model: Apple II, II Plus, IIe, or IIe Enhanced (WDC 65C02)

   for Apple_2_Model use
     (Apple_2           => 0, Apple_2_Plus => 1, Apple_2e => 16,
      Apple_2e_Enhanced => 17);
   --  Define to use the same settings values as LinApple

   type Video_Type_Type is
     (Mono_Custom, Color_Standard, Color_Text_Optimized, Color_TV_Emu,
      Color_Half_Shift_Dim, Mono_Amber, Mono_Green, Mono_White);

   type Video_Standard_Type is (NTSC, PAL);
   --  Use NTSC or PAL timings for video

   type Sound_Card_Type is (Sound_Builtin, Sound_Mockingboard, Sound_Phasor);
   --  Mockingboard, Phasor, or no sound card

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

   type Apple_2_Speed is range 1 .. 40;  --  CPU MHz x10 = 0.1 .. 4.0 MHz

   Speed_Min    : constant Apple_2_Speed := 1;   --  0.1 MHz min speed
   Speed_Normal : constant Apple_2_Speed := 10;  --  1.0 MHz base speed
   Speed_Max    : constant Apple_2_Speed := 40;  --  4.0 MHz max speed

   type Draw_Config_Flags is mod 256;

   Draw_Background    : constant Draw_Config_Flags := 16#01#;
   Draw_LEDs          : constant Draw_Config_Flags := 16#02#;
   Draw_Title         : constant Draw_Config_Flags := 16#04#;
   Draw_Button_Drives : constant Draw_Config_Flags := 16#08#;

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

   type Mem_Mode_Flags is mod 2**16;
   --  Memory map behavior flags

   Flag_80_Store        : constant Mem_Mode_Flags := 16#0001#;
   Flag_Alt_ZP          : constant Mem_Mode_Flags := 16#0002#;
   Flag_Aux_Read        : constant Mem_Mode_Flags := 16#0004#;
   Flag_Aux_Write       : constant Mem_Mode_Flags := 16#0008#;
   Flag_High_RAM_Bank_2 : constant Mem_Mode_Flags := 16#0010#;
   Flag_High_RAM        : constant Mem_Mode_Flags := 16#0020#;
   Flag_Hi_Res          : constant Mem_Mode_Flags := 16#0040#;
   Flag_Page_2          : constant Mem_Mode_Flags := 16#0080#;
   Flag_Slot_C3_ROM     : constant Mem_Mode_Flags := 16#0100#;
   Flag_Slot_CX_ROM     : constant Mem_Mode_Flags := 16#0200#;
   Flag_High_RAM_Write  : constant Mem_Mode_Flags := 16#0400#;

   type Mem_Init_Pattern_Type is (Pattern_Zero, Pattern_FF_FF_00_00);

   type IRQ_Source is (IRQ_6522, IRQ_Speech, IRQ_SSC, IRQ_MOUSE);

   type Button_ID is (Button_0, Button_1);
   type Button_State is (Button_Up, Button_Down);

   type Video_Mode_Flags is mod 2**16;

   Video_Flag_80_Column : constant Video_Mode_Flags := 16#0001#;
   Video_Flag_Dbl_Hires : constant Video_Mode_Flags := 16#0002#;
   Video_Flag_Hires     : constant Video_Mode_Flags := 16#0004#;
   Video_Flag_Mask_2    : constant Video_Mode_Flags := 16#0008#;
   Video_Flag_Mixed     : constant Video_Mode_Flags := 16#0010#;
   Video_Flag_Page_2    : constant Video_Mode_Flags := 16#0020#;
   Video_Flag_Text      : constant Video_Mode_Flags := 16#0040#;

   type Mem_Page_Table is array (Unsigned_8) of RAM_Bank_Index;
   --  Table of RAM bank to use within RAM_Pages for each page in 64K

   type Joystick_Device is
     (Joy_None, Joy_Joystick, Joy_Keyboard, Joy_Keyboard_Centered, Joy_Mouse);
   --  Apple joysticks can be emulated with joystick, keyboard, or mouse

   type Joystick_Range is range 0 .. 1;
   --  Apple joystick numbers go from 0 to 1

   type Joystick_Devices is array (Joystick_Range) of Joystick_Device;

   type Joystick_Button_Range is range 0 .. 2;

   type Joystick_Button_States is array (Joystick_Button_Range) of Boolean;

   type SDL_Joystick_Devices is
     array (Joystick_Range) of SDL.Inputs.Joysticks.All_Devices;
   --  For each Apple II joystick, either 0 or an SDLAda joystick ID (1 .. n)

   type Disk_2_Range is range 0 .. 1;
   --  Apple disk drive numbers go from 0 to 1

   type Disk_2_Filenames is array (Disk_2_Range) of Unbounded_String;
   --  Pathnames for the two emulated disk drives

   -----------------------
   -- Emulator Settings --
   -----------------------

   type Settings_Type is record

      --  Emulator preferences

      Slot_6_Dir, Save_State_Dir, Save_State_Filename : Unbounded_String;

      Show_LEDs : Boolean := True;  --  show drive LEDs by default

      Boot_At_Startup : Boolean := False;  --  false = show splash screen

      Fullscreen : Boolean := False;  --  true = start in fullscreen mode

      --  General emulation settings

      Model : Apple_2_Model := Apple_2e_Enhanced;  --  computer type

      Speed : Apple_2_Speed := Speed_Normal;  --  MHz x10

      --  Video settings

      Video_Type : Video_Type_Type := Color_Standard;

      Video_Standard : Video_Standard_Type := NTSC;

      Mono_Color : RGB_Colour := RGB_Colour'(16#C0#, 16#C0#, 16#C0#);

      --  Audio settings

      Sound_Card : Sound_Card_Type := Sound_Mockingboard;

      --  Disk II settings

      Disk_Image_Filenames : Disk_2_Filenames;

      --  Keyboard settings

      Keyboard_Rocker_Switch : Boolean := False;

      --  Joystick / paddle settings

      Joystick_Types : Joystick_Devices :=
        Joystick_Devices'(Joy_Joystick, Joy_None);
      --  Note: GNAT 13.2.0 (x86_64-pc-linux-gnu) crashes with "Storage_Error
      --  stack overflow or erroneous memory access" if "Joystick_Devices'"
      --  is removed from the assignment above! At least it works this way.

      SDL_Joystick_IDs : SDL_Joystick_Devices := (others => 0);

      --  Printer settings

      Printer_Filename : Unbounded_String :=
        To_Unbounded_String ("Printer.txt");

      Printer_Idle_Timeout : Unsigned_32 := 10;

      Printer_Append_To_File : Boolean := True;

   end record;
   --  All of the user settings that are saved to the config file

   -------------------------------------
   -- Apple II/II+/IIe Computer State --
   -------------------------------------

   type Apple2_Base is abstract new CPU_6502_Series with record

      Settings : Settings_Type;
      --  All of the configuration settings saved to the registry

      Mem_Mode : Mem_Mode_Flags :=
        Flag_High_RAM_Bank_2 or Flag_Slot_CX_ROM or Flag_High_RAM_Write;
      --  Memory mode flags (initialized to startup value)

      Mem_Init_Pattern : Mem_Init_Pattern_Type := Pattern_FF_FF_00_00;
      --  Current memory init pattern

      Mem_Read_Bank : Mem_Page_Table := (others => RAM_Bank_Main);
      --  Memory read paging table (no bank specified)

      Mem_Write_Bank : Mem_Page_Table := (others => RAM_Bank_Main);
      --  Memory write paging table

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

      Video_Mode : Video_Mode_Flags := 0;
      --  Current video mode

      Button_State : Joystick_Button_States := (others => False);
      --  State of the three joystick buttons

   end record;
   --  Holds the state for an Apple II variant with devices, except RAM/ROM

   function Mem_Read
     (Mem     : not null access constant RAM_All_Banks; Bank : RAM_Bank_Index;
      Address : Unsigned_16) return Unsigned_8 with
     Inline;
   --  Read a byte from memory by bank and 16-bit address

   procedure Mem_Write
     (C    : in out Apple2_Base; Mem : not null access RAM_All_Banks;
      Bank :    RAM_Bank_Index; Address : Unsigned_16; Value : Unsigned_8) with
     Inline;
   --  Write a byte to memory by bank and 16-bit address

   function Is_Apple2 (C : Apple2_Base) return Boolean with
     Inline;
   --  Is this an original Apple II or II Plus?

   function Get_Apple_2_Model (C : Apple2_Base) return Apple_2_Model with
     Inline;
   --  Returns the current machine type

   procedure CPU_Execute
     (C            : in out Apple2_Base; Mem : not null access RAM_All_Banks;
      Total_Cycles :        Natural) with
     Inline;
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

   --  sizes of status panel

   Status_Panel_Width  : constant := 100;
   Status_Panel_Height : constant := 48;

private
   --  TODO: Remove the following pragmas when video emulation is ported

   pragma Unreferenced (Video_Flag_Dbl_Hires);
   pragma Unreferenced (Video_Flag_Hires);
   pragma Unreferenced (Video_Flag_Mask_2);
   pragma Unreferenced (Video_Flag_Mixed);
   pragma Unreferenced (Video_Flag_Page_2);
   pragma Unreferenced (Video_Flag_Text);

end Apple2;
