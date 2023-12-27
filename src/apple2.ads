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

   Apple_Slot_Size : constant := 256;
   --  1 page = $Cx00 .. $CxFF (slot 1 .. 7)

   Expansion_ROM_Size : constant := 2 * 1_024;
   --  expansion ROM size

   Apple_2_ROM_Size : constant := 12 * 1_024;
   --  12K ROM (II / II Plus)

   Extra_ROM_Size : constant := 4 * 1_024;
   --  4K ROM at $C000 (IIe)

   Apple_2e_ROM_Size : constant := Apple_2_ROM_Size + Extra_ROM_Size;
   --  16K ROM (IIe / IIe Enhanced)

   subtype System_ROM_Type is Mem_Byte_Range (0 .. Apple_2_ROM_Size - 1);
   --  12K ROM

   subtype System_ROM_2e_Type is Mem_Byte_Range (0 .. Apple_2e_ROM_Size - 1);
   --  16K ROM

   subtype Extra_ROM_Type is Mem_Byte_Range (0 .. Extra_ROM_Size - 1);
   --  4K Apple IIe bank-switched ROM

   subtype Card_ROM_Type is Mem_Byte_Range (0 .. Apple_Slot_Size - 1);
   --  256-byte card ROM

   subtype Expansion_ROM_Type is Mem_Byte_Range (0 .. Expansion_ROM_Size - 1);
   --  2K expansion ROM

   type Expansion_ROM_Access is access constant Expansion_ROM_Type;
   --  2K expansion ROM

   ------------------------------------------------------
   -- Clock speeds / durations for NTSC & PAL machines --
   ------------------------------------------------------

   NTSC_M14 : constant := 14_318_182;
   --  NTSC M14 clock speed in Hz

   PAL_M14 : constant := 14_250_450;
   --  PAL M14 clock speed in Hz

   Cycles_Per_Line : constant := 65;
   --  25 cycles of HBL & 40 cycles of display output

   Visible_Lines_Per_Frame : constant := 192;
   --  64 lines in each third of the screen

   NTSC_Lines_Per_Frame : constant := Visible_Lines_Per_Frame + 70;
   --  64 in each third of the screen & 70 in VBL

   NTSC_Clocks_Per_Frame : constant := Cycles_Per_Line * NTSC_Lines_Per_Frame;
   --  NTSC clocks per frame = 17030

   PAL_Lines_Per_Frame : constant := Visible_Lines_Per_Frame + 120;
   --  64 in each third of the screen & 120 in VBL

   PAL_Clocks_Per_Frame : constant := Cycles_Per_Line * PAL_Lines_Per_Frame;
   --  PAL clocks per frame = 20280

   --  The next two types will be removed if I don't use them. I'm using
   --  Unsigned_16 for scan lines and columns in the 6502 CPU emulation
   --  because I don't want it to depend on any Apple II-specific details.

   type Scan_Line_Count is range 0 .. PAL_Lines_Per_Frame;
   --  Define a type with a range big enough for PAL or NTSC, plus one for
   --  overflow from the CPU incrementing past the last scan line.

   type Column_Cycle_Count is range 0 .. Cycles_Per_Line - 1;
   --  Define a type to hold the current cycle within a line

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

   type Joystick_Device is
     (Joy_None, Joy_Joystick, Joy_Keyboard, Joy_Keyboard_Centered, Joy_Mouse);
   --  Apple joysticks can be emulated with joystick, keyboard, or mouse

   type Joystick_Index is range 0 .. 1;
   --  Apple joystick numbers go from 0 to 1

   type Joystick_Devices is array (Joystick_Index) of Joystick_Device;

   type Joystick_Axis is range 0 .. 3;

   type Joystick_Button is range 0 .. 2;

   type Joystick_Button_States is array (Joystick_Button) of Boolean;

   type SDL_Joystick_Devices is
     array (Joystick_Index) of SDL.Inputs.Joysticks.All_Devices;
   --  For each Apple II joystick, either 0 or an SDLAda joystick ID (1 .. n)

   type Disk_2_Index is range 0 .. 1;
   --  Apple disk drive numbers go from 0 to 1

   type Disk_2_Filenames is array (Disk_2_Index) of Unbounded_String;
   --  Pathnames for the two emulated disk drives

   type Mode_Flags is record
      Mem_80_Store             : Boolean;
      Mem_Alt_ZP               : Boolean;
      Mem_Aux_Read             : Boolean;
      Mem_Aux_Write            : Boolean;
      Mem_Expansion_ROM_Active : Boolean;
      Mem_High_RAM             : Boolean;
      Mem_High_RAM_Bank_2      : Boolean;
      Mem_High_RAM_Write       : Boolean;
      Mem_Slot_C3_ROM          : Boolean;
      Mem_Slot_CX_ROM          : Boolean;
      Video_80_Column          : Boolean;
      Video_Text               : Boolean;
      Video_Mixed              : Boolean;
      Video_Page_2             : Boolean;
      Video_Hi_Res             : Boolean;
      Video_Dbl_Hi_Res         : Boolean;
      Video_Dbl_Hi_Res_Visible : Boolean;
      Video_Alt_Charset        : Boolean;
   end record with
     Pack;
   --  MMU, video, I/O, and other soft switch states and mode flags

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

      Video_Type     : Video_Type_Type     := Color_Standard;
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

      Printer_Idle_Timeout   : Unsigned_32 := 10;
      Printer_Append_To_File : Boolean     := True;

   end record;
   --  All of the user settings that are saved to the config file

   -------------------------------------
   -- Apple II/II+/IIe Computer State --
   -------------------------------------

   type Apple2_Base is abstract new CPU_6502_Series with record

      Settings : Settings_Type;
      --  All of the configuration settings saved to the registry

      Mode : Mode_Flags :=
        (Mem_High_RAM_Bank_2 => True, Mem_Slot_CX_ROM => True,
         Mem_High_RAM_Write  => True, Video_Text => True, others => False);
      --  Soft switch and other mode flags (initialized to startup values)

      Frames_Since_Boot : CPU_Cycle_Count := 0;
      --  64-bit frame count, updated by the loop that calls CPU_Execute

      Starting_Cycle : CPU_Cycle_Count := 0;
      --  Offset of first actual emulated CPU cycle (start of first VBL)

      Button_State : Joystick_Button_States := (others => False);
      --  State of the three joystick buttons

      Spkr_First_Click_Cycle, Spkr_Last_Click_Cycle : CPU_Cycle_Count := 0;
      --  Cycle count of first and last speaker clicks seen this frame

      Spkr_Num_Clicks : Natural := 0;
      --  Number of speaker clicks seen this frame

   end record;
   --  Holds the state for an Apple II variant with devices, except RAM/ROM

   function Is_Apple2 (C : Apple2_Base) return Boolean with
     Inline;
   --  Is this an original Apple II or II Plus?

   function Get_Apple_2_Model (C : Apple2_Base) return Apple_2_Model with
     Inline;
   --  Returns the current machine type

   function Cycles_Since_Boot (C : Apple2_Base) return CPU_Cycle_Count with
     Inline;
   --  Returns the number of clock cycles since boot

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

end Apple2;
