--  Ada-Apple2 : An Apple //e emulator in Ada
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

--  This package handles loading and saving the emulator settings.
--  The file I/O and config line parsing is handled by Emu.Registry.
--  We actually have two copies of every setting, one in the Registry hash map
--  and one in the Settings_Type record. The Init_Registry procedure in this
--  package adds default values and types so that the call to Load_Settings
--  will only load keys that we know about, using the expected data types.
--  The call to Init_Settings copies the values back to the Settings object.

with Emu.Registry; use Emu.Registry;

with SDL.Inputs.Joysticks; use SDL.Inputs.Joysticks;

package Apple2.Settings with
  SPARK_Mode => Off
is

   procedure Init_Registry (S : Settings_Type; Reg : in out Registry_Type);
   --  Set up the registry with default values for everything we care about

   procedure Init_Settings (S : in out Settings_Type; Reg : Registry_Type);
   --  Initialize the settings object with values loaded from the registry

   ---------------------------------------------------
   -- Procedures to update settings and config file --
   ---------------------------------------------------

   procedure Set_Slot_6_Dir
     (S : in out Settings_Type; Reg : in out Registry_Type; New_Dir : String);
   --  Set the current slot 6 directory

   procedure Set_Save_State_Dir
     (S : in out Settings_Type; Reg : in out Registry_Type; New_Dir : String);
   --  Set the current save state directory

   procedure Set_Save_State_Filename
     (S : in out Settings_Type; Reg : in out Registry_Type; New_Name : String);
   --  Set the current save state filename (with path)

   procedure Set_Show_LEDs
     (S          : in out Settings_Type; Reg : in out Registry_Type;
      New_Enable :        Boolean);
   --  Enable showing drive LEDs on screen

   procedure Set_Boot_At_Startup
     (S          : in out Settings_Type; Reg : in out Registry_Type;
      New_Enable :        Boolean);
   --  Enable whether to skip splash screen at startup

   procedure Set_Fullscreen
     (S          : in out Settings_Type; Reg : in out Registry_Type;
      New_Enable :        Boolean);
   --  Enable fullscreen mode at startup

   procedure Set_Apple_2_Model
     (S         : in out Settings_Type; Reg : in out Registry_Type;
      New_Model :        Apple_2_Model);
   --  Set machine type from the default Apple IIe Enhanced

   procedure Set_Emu_Speed
     (S         : in out Settings_Type; Reg : in out Registry_Type;
      New_Speed :        Apple_2_Speed);
   --  Set emulation speed in MHz x10 (0.1 .. 4.0 MHz)

   procedure Set_Video_Type
     (S        : in out Settings_Type; Reg : in out Registry_Type;
      New_Type :        Video_Type_Type);
   --  Set the video emulation type

   procedure Set_Video_Standard
     (S            : in out Settings_Type; Reg : in out Registry_Type;
      New_Standard :        Video_Standard_Type);
   --  Set the video standard (NTSC / PAL)

   procedure Set_Monochrome_Color
     (S         : in out Settings_Type; Reg : in out Registry_Type;
      New_Color :        SDL.Video.Palettes.RGB_Colour);
   --  Set the monochrome color to use

   procedure Set_Sound_Card
     (S             : in out Settings_Type; Reg : in out Registry_Type;
      New_Card_Type :        Sound_Card_Type);
   --  Set the sound card to emulate (Mockingboard, Phasor, or none)

   procedure Set_Disk_Image_Filename
     (S     : in out Settings_Type; Reg : in out Registry_Type;
      Drive :        Disk_2_Index; New_Name : String);
   --  Set the disk image filename to use for drive 0 or 1

   procedure Set_Keyboard_Rocker_Switch
     (S          : in out Settings_Type; Reg : in out Registry_Type;
      New_Enable :        Boolean);
   --  Enable rocker switch for alternate keyboard character set

   procedure Set_Joystick_Type
     (S      : in out Settings_Type; Reg : in out Registry_Type;
      Number :        Joystick_Index; New_Type : Joystick_Device);
   --  Set the device type to use to emulate joystick 0 or 1

   procedure Set_SDL_Joystick_ID
     (S      : in out Settings_Type; Reg : in out Registry_Type;
      Number :        Joystick_Index; New_ID : All_Devices);
   --  Set the SDL joystick ID (1 .. n) to emulate joystick 0 or 1

   procedure Set_Printer_Filename
     (S : in out Settings_Type; Reg : in out Registry_Type; New_Name : String);
   --  Set the parallel printer output filename

   procedure Set_Printer_Idle_Timeout
     (S           : in out Settings_Type; Reg : in out Registry_Type;
      New_Timeout :        Unsigned_32);
   --  Set the timeout in seconds before the printer output file is closed

   procedure Set_Printer_Append_To_File
     (S : in out Settings_Type; Reg : in out Registry_Type; Append : Boolean);
   --  Set to true to append to existing printer file; false to overwrite

end Apple2.Settings;
