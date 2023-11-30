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

package Apple2 is

   type Value_8_Bit is mod 256;
   --  8-bit value type

   pragma Provide_Shift_Operators (Value_8_Bit);
   --  GNAT pragma to provide shift and rotate functions

   type Address_16_Bit is mod 2 ** 16;
   --  16-bit memory address or address range size in emulated address space

   pragma Provide_Shift_Operators (Address_16_Bit);
   --  GNAT pragma to provide shift and rotate functions

   type Value_4_Bit is mod 16;
   --  4-bit range for case statements over 16-byte ranges

   type Value_32_Bit is mod 2 ** 32;
   --  32-bit unsigned value

   pragma Provide_Shift_Operators (Value_32_Bit);
   --  GNAT pragma to provide shift and rotate functions

   type Mem_Range is array (Address_16_Bit range <>) of Value_8_Bit;
   --  Type for a range of 8-bit memory

   type Mem_Range_Access is access all Mem_Range;
   --  Access type for a range of 8-bit memory

   subtype Mem_Page is Mem_Range (Address_16_Bit range 0 .. 255);
   --  Type for a 256-byte page of memory

   type Mem_Page_Access is access all Mem_Page;
   --  Access type for a 256-byte memory page

   subtype Mem_Bank_64K is Mem_Range (Address_16_Bit range 0 .. 16#FFFF#);
   --  Type for a 64K bank of memory

   type Mem_Bank_Access is access all Mem_Bank_64K;
   --  Access type for a 64K bank of memory

   Cx_ROM_Size : constant := 4 * 1024;

   Apple_2_ROM_Size : constant := 12 * 1024;

   Apple_2e_ROM_Size : constant := Apple_2_ROM_Size + Cx_ROM_Size;

   M14 : constant Long_Float := 157_500_000.0 / 11.0;
   --  14.3181818... * 10^6

   Clock_6502 : constant Long_Float := (M14 * 65.0) / 912.0;
   --  65 cycles per 912 14M clocks

   Clock_Z80  : constant Long_Float := Clock_6502 * 2.0;
   --  effective Z80 clock rate is 2.041 MHz

   Cycles_Per_Line : constant := 65;
   --  25 cycles of HBL & 40 cycles of HBL

   Visible_Lines_Per_Frame : constant := 64 * 3; --  192

   Lines_Per_Frame : constant := 262;
   --  64 in each third of the screen & 70 in VBL

   Clocks_Per_Frame : constant := Cycles_Per_Line * Lines_Per_Frame;
   --  17030

   Num_Slots : constant := 8;

   type Slot_Range is range 0 .. Num_Slots - 1;

   --  Use a base freq so that the OS is less likely to need to resample.
   --  Assume base freqs are 44.1KHz & 48KHz.

   Speaker_Sample_Rate : constant := 44_100;
   --  for Apple ][ speaker
   Sample_Rate         : constant := 44_100;
   --  for Phasor/Mockingboard

   type App_Mode is
     (Logo,
      Paused,
      Running,    --  6502 running at normal speed (breakpoints may be active)
      Debug,      --  6502 paused
      Stepping);  --  6502 running at full speed (breakpoints always active)

   Speed_Min    : constant := 0;
   Speed_Normal : constant := 10;
   Speed_Max    : constant := 40;

   type Draw_Config_Type is private;

   Draw_Background    : constant Draw_Config_Type;
   Draw_LEDs          : constant Draw_Config_Type;
   Draw_Title         : constant Draw_Config_Type;
   Draw_Button_Drives : constant Draw_Config_Type;

   --  Function Keys F1 - F12

   type Func_Key_Type is private;

   Func_Key_Help        : constant Func_Key_Type;
   Func_Key_Run         : constant Func_Key_Type;
   Func_Key_Drive1      : constant Func_Key_Type;
   Func_Key_Drive2      : constant Func_Key_Type;
   Func_Key_Drive_Swap  : constant Func_Key_Type;
   Func_Key_Full_Screen : constant Func_Key_Type;
   Func_Key_Debug       : constant Func_Key_Type;
   Func_Key_Setup       : constant Func_Key_Type;
   Func_Key_Cycle       : constant Func_Key_Type;
   Func_Key_Quit        : constant Func_Key_Type;
   Func_Key_Save_State  : constant Func_Key_Type;
   Func_Key_Load_State  : constant Func_Key_Type;

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

   --  For FTP access

   Reg_Value_FTP_Dir     : constant String := "FTP Server";
   Reg_Value_FTP_HDD_Dir : constant String := "FTP ServerHDD";

   Reg_Value_FTP_Local_Dir     : constant String := "FTP Local Dir";
   Reg_Value_FTP_User_Password : constant String := "FTP UserPass";

   type IRQ_Source is (IRQ_6522, IRQ_Speech, IRQ_SSC, IRQ_MOUSE);

   type Apple_2_Type is mod 256;
   --  These get persisted to the Registry, so don't change the constant values

   Apple_2           : constant Apple_2_Type := 0;
   Apple_2_Plus      : constant Apple_2_Type := 1;
   Apple_2e          : constant Apple_2_Type := 16;
   Apple_2e_Enhanced : constant Apple_2_Type := 17;

   function Is_Apple2 return Boolean;
   --  Returns true for original II and II+; false for IIe and IIe Enhanced

   function Get_Machine_Type return Apple_2_Type;
   --  Returns the current machine type

   procedure Set_Machine_Type (New_Type : Apple_2_Type);
   --  Change machine type from the default Apple IIe Enhanced

   type Button_ID is (Button_0, Button_1);
   type Button_State is (Button_Up, Button_Down);

   --  sizes of status panel

   Status_Panel_Width  : constant := 100;
   Status_Panel_Height : constant := 48;

private

   type Draw_Config_Type is mod 256;

   Draw_Background    : constant Draw_Config_Type := 16#01#;
   Draw_LEDs          : constant Draw_Config_Type := 16#02#;
   Draw_Title         : constant Draw_Config_Type := 16#04#;
   Draw_Button_Drives : constant Draw_Config_Type := 16#08#;

   type Func_Key_Type is range 0 .. 11;

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

end Apple2;
