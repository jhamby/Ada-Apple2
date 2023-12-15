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

package body Apple2.Settings with
  SPARK_Mode => Off
is
   --  Emulator preferences

   Reg_Key_Slot_6_Dir : constant String := "Slot 6 Directory";

   Reg_Key_Save_State_Dir : constant String := "Save State Directory";

   Reg_Key_Save_State_Filename : constant String := "Save State Filename";

   Reg_Key_Show_LEDs : constant String := "Show Leds";

   Reg_Key_Boot_At_Startup : constant String := "Boot at Startup";

   Reg_Key_Fullscreen : constant String := "Fullscreen";

   --  General emulation settings

   Reg_Key_Model : constant String := "Computer Emulation";

   Reg_Key_Emu_Speed : constant String := "Emulation Speed";

   --  Video settings

   Reg_Key_Video_Emulation : constant String := "Video Emulation";

   Reg_Key_Video_Standard : constant String := "Video Standard";

   Reg_Key_Monochrome_Color : constant String := "Monochrome Color";

   --  Audio settings

   Reg_Key_Sound_Card_Type : constant String := "Soundcard Type";

   --  Disk II settings

   Reg_Key_Disk_Image_1 : constant String := "Disk Image 1";

   Reg_Key_Disk_Image_2 : constant String := "Disk Image 2";

   --  Keyboard settings

   Reg_Key_KB_Rocker_Switch : constant String := "Keyboard Rocker Switch";

   --  Joystick / paddle settings

   Reg_Key_Joy_Type_0 : constant String := "Joystick 0";
   Reg_Key_Joy_Type_1 : constant String := "Joystick 1";

   Reg_Key_Joy_0_Index : constant String := "Joy0Index";
   Reg_Key_Joy_1_Index : constant String := "Joy1Index";

   --  Printer settings

   Reg_Key_Printer_Filename : constant String := "Parallel Printer Filename";

   Reg_Key_Printer_Idle_Limit : constant String := "Printer idle limit";

   Reg_Key_Printer_Append : constant String := "Append to printer file";

   -------------------
   -- Init_Registry --
   -------------------

   procedure Init_Registry (S : Settings_Type; Reg : in out Registry_Type) is
   begin
      Add_String (Reg, Reg_Key_Slot_6_Dir, To_String (S.Slot_6_Dir));
      Add_String (Reg, Reg_Key_Save_State_Dir, To_String (S.Save_State_Dir));

      Add_String
        (Reg, Reg_Key_Save_State_Filename, To_String (S.Save_State_Filename));

      Add_Boolean (Reg, Reg_Key_Show_LEDs, S.Show_LEDs);
      Add_Boolean (Reg, Reg_Key_Boot_At_Startup, S.Boot_At_Startup);
      Add_Boolean (Reg, Reg_Key_Fullscreen, S.Fullscreen);

      Add_Value (Reg, Reg_Key_Model, Apple_2_Model'Enum_Rep (S.Model));

      Add_Value (Reg, Reg_Key_Emu_Speed, Unsigned_32 (S.Speed));

      Add_Value
        (Reg, Reg_Key_Video_Emulation, Video_Type_Type'Pos (S.Video_Type));
      Add_Value
        (Reg, Reg_Key_Video_Standard,
         Video_Standard_Type'Pos (S.Video_Standard));

      declare
         Color_String : String := "#xxxxxx";
      begin
         Put_Hex_Byte (Color_String (2 .. 3), Unsigned_8 (S.Mono_Color.Red));
         Put_Hex_Byte (Color_String (4 .. 5), Unsigned_8 (S.Mono_Color.Green));
         Put_Hex_Byte (Color_String (6 .. 7), Unsigned_8 (S.Mono_Color.Blue));
         Add_String (Reg, Reg_Key_Monochrome_Color, Color_String);
      end;

      Add_Value
        (Reg, Reg_Key_Sound_Card_Type, Sound_Card_Type'Pos (S.Sound_Card));

      Add_String
        (Reg, Reg_Key_Disk_Image_1, To_String (S.Disk_Image_Filenames (0)));

      Add_String
        (Reg, Reg_Key_Disk_Image_2, To_String (S.Disk_Image_Filenames (1)));

      Add_Boolean (Reg, Reg_Key_KB_Rocker_Switch, S.Keyboard_Rocker_Switch);

      Add_Value
        (Reg, Reg_Key_Joy_Type_0, Joystick_Device'Pos (S.Joystick_Types (0)));
      Add_Value
        (Reg, Reg_Key_Joy_Type_1, Joystick_Device'Pos (S.Joystick_Types (1)));

      Add_Value
        (Reg, Reg_Key_Joy_0_Index, Unsigned_32 (S.SDL_Joystick_IDs (0)));
      Add_Value
        (Reg, Reg_Key_Joy_1_Index, Unsigned_32 (S.SDL_Joystick_IDs (1)));

      Add_String
        (Reg, Reg_Key_Printer_Filename, To_String (S.Printer_Filename));

      Add_Value (Reg, Reg_Key_Printer_Idle_Limit, S.Printer_Idle_Timeout);

      Add_Boolean (Reg, Reg_Key_Printer_Append, S.Printer_Append_To_File);

   end Init_Registry;

   -------------------
   -- Init_Settings --
   -------------------

   procedure Init_Settings (S : in out Settings_Type; Reg : Registry_Type) is
      Value : Unsigned_32;
   begin
      Set_Unbounded_String
        (S.Slot_6_Dir, Get_String (Reg, Reg_Key_Slot_6_Dir));
      Set_Unbounded_String
        (S.Save_State_Dir, Get_String (Reg, Reg_Key_Save_State_Dir));

      Set_Unbounded_String
        (S.Save_State_Filename, Get_String (Reg, Reg_Key_Save_State_Filename));

      S.Show_LEDs       := Get_Boolean (Reg, Reg_Key_Show_LEDs);
      S.Boot_At_Startup := Get_Boolean (Reg, Reg_Key_Boot_At_Startup);
      S.Fullscreen      := Get_Boolean (Reg, Reg_Key_Fullscreen);

      begin
         S.Model := Apple_2_Model'Enum_Val (Get_Value (Reg, Reg_Key_Model));
      exception
         when Constraint_Error =>
            null;
      end;

      Value := Get_Value (Reg, Reg_Key_Emu_Speed);
      if Value >= Unsigned_32 (Apple_2_Speed'First) and
        Value <= Unsigned_32 (Apple_2_Speed'Last)
      then
         S.Speed := Apple_2_Speed (Value);
      end if;

      Value := Get_Value (Reg, Reg_Key_Video_Emulation);
      if Value <= Video_Type_Type'Pos (Video_Type_Type'Last) then
         S.Video_Type := Video_Type_Type'Val (Value);
      end if;

      Value := Get_Value (Reg, Reg_Key_Video_Standard);
      if Value <= Video_Standard_Type'Pos (Video_Standard_Type'Last) then
         S.Video_Standard := Video_Standard_Type'Val (Value);
      end if;

      declare
         Color_String : constant String :=
           Get_String (Reg, Reg_Key_Monochrome_Color);
         New_Color    : RGB_Colour      := Null_RGB_Colour;
         Component    : Unsigned_8      := 0;
         Success      : Boolean         := True;
      begin
         if Color_String'Length = 7 and then Color_String (1) = '#' then
            Get_Hex_Byte (Color_String (2 .. 3), Component, Success);
            if not Success then
               goto Not_A_Color;
            end if;
            New_Color.Red := Colour_Component (Component);

            Get_Hex_Byte (Color_String (4 .. 5), Component, Success);
            if not Success then
               goto Not_A_Color;
            end if;
            New_Color.Green := Colour_Component (Component);

            Get_Hex_Byte (Color_String (6 .. 7), Component, Success);
            if not Success then
               goto Not_A_Color;
            end if;
            New_Color.Blue := Colour_Component (Component);

            S.Mono_Color := New_Color;
         end if;
      end;
      <<Not_A_Color>>

      Value := Get_Value (Reg, Reg_Key_Sound_Card_Type);
      if Value <= Sound_Card_Type'Pos (Sound_Card_Type'Last) then
         S.Sound_Card := Sound_Card_Type'Val (Value);
      end if;

      Set_Unbounded_String
        (S.Disk_Image_Filenames (0), Get_String (Reg, Reg_Key_Disk_Image_1));

      Set_Unbounded_String
        (S.Disk_Image_Filenames (1), Get_String (Reg, Reg_Key_Disk_Image_2));

      S.Keyboard_Rocker_Switch := Get_Boolean (Reg, Reg_Key_KB_Rocker_Switch);

      Value := Get_Value (Reg, Reg_Key_Joy_Type_0);
      if Value <= Joystick_Device'Pos (Joystick_Device'Last) then
         S.Joystick_Types (0) := Joystick_Device'Val (Value);
      end if;

      Value := Get_Value (Reg, Reg_Key_Joy_Type_1);
      if Value <= Joystick_Device'Pos (Joystick_Device'Last) then
         S.Joystick_Types (1) := Joystick_Device'Val (Value);
      end if;

      Value := Get_Value (Reg, Reg_Key_Joy_0_Index);
      if Value <= Unsigned_32 (All_Devices'Last) then
         S.SDL_Joystick_IDs (0) := All_Devices (Value);
      end if;

      Value := Get_Value (Reg, Reg_Key_Joy_1_Index);
      if Value <= Unsigned_32 (All_Devices'Last) then
         S.SDL_Joystick_IDs (1) := All_Devices (Value);
      end if;

      Set_Unbounded_String
        (S.Printer_Filename, Get_String (Reg, Reg_Key_Printer_Filename));

      S.Printer_Idle_Timeout := Get_Value (Reg, Reg_Key_Printer_Idle_Limit);

      S.Printer_Append_To_File := Get_Boolean (Reg, Reg_Key_Printer_Append);

   end Init_Settings;

   --------------------
   -- Set_Slot_6_Dir --
   --------------------

   procedure Set_Slot_6_Dir
     (S : in out Settings_Type; Reg : in out Registry_Type; New_Dir : String)
   is
   begin
      Set_Unbounded_String (S.Slot_6_Dir, New_Dir);
      Save_String (Reg, Reg_Key_Slot_6_Dir, New_Dir);
   end Set_Slot_6_Dir;

   ------------------------
   -- Set_Save_State_Dir --
   ------------------------

   procedure Set_Save_State_Dir
     (S : in out Settings_Type; Reg : in out Registry_Type; New_Dir : String)
   is
   begin
      Set_Unbounded_String (S.Save_State_Dir, New_Dir);
      Save_String (Reg, Reg_Key_Save_State_Dir, New_Dir);
   end Set_Save_State_Dir;

   -----------------------------
   -- Set_Save_State_Filename --
   -----------------------------

   procedure Set_Save_State_Filename
     (S : in out Settings_Type; Reg : in out Registry_Type; New_Name : String)
   is
   begin
      Set_Unbounded_String (S.Save_State_Filename, New_Name);
      Save_String (Reg, Reg_Key_Save_State_Filename, New_Name);
   end Set_Save_State_Filename;

   -------------------
   -- Set_Show_LEDs --
   -------------------

   procedure Set_Show_LEDs
     (S          : in out Settings_Type; Reg : in out Registry_Type;
      New_Enable :        Boolean)
   is
   begin
      S.Show_LEDs := New_Enable;
      Save_Boolean (Reg, Reg_Key_Show_LEDs, New_Enable);
   end Set_Show_LEDs;

   -------------------------
   -- Set_Boot_At_Startup --
   -------------------------

   procedure Set_Boot_At_Startup
     (S          : in out Settings_Type; Reg : in out Registry_Type;
      New_Enable :        Boolean)
   is
   begin
      S.Boot_At_Startup := New_Enable;
      Save_Boolean (Reg, Reg_Key_Boot_At_Startup, New_Enable);
   end Set_Boot_At_Startup;

   --------------------
   -- Set_Fullscreen --
   --------------------

   procedure Set_Fullscreen
     (S          : in out Settings_Type; Reg : in out Registry_Type;
      New_Enable :        Boolean)
   is
   begin
      S.Fullscreen := New_Enable;
      Save_Boolean (Reg, Reg_Key_Fullscreen, New_Enable);
   end Set_Fullscreen;

   -----------------------
   -- Set_Apple_2_Model --
   -----------------------

   procedure Set_Apple_2_Model
     (S         : in out Settings_Type; Reg : in out Registry_Type;
      New_Model :        Apple_2_Model)
   is
   begin
      S.Model := New_Model;
      Save_Value (Reg, Reg_Key_Model, Apple_2_Model'Enum_Rep (New_Model));
   end Set_Apple_2_Model;

   -------------------
   -- Set_Emu_Speed --
   -------------------

   procedure Set_Emu_Speed
     (S         : in out Settings_Type; Reg : in out Registry_Type;
      New_Speed :        Apple_2_Speed)
   is
   begin
      S.Speed := New_Speed;
      Save_Value (Reg, Reg_Key_Emu_Speed, Unsigned_32 (New_Speed));
   end Set_Emu_Speed;

   --------------------
   -- Set_Video_Type --
   --------------------

   procedure Set_Video_Type
     (S        : in out Settings_Type; Reg : in out Registry_Type;
      New_Type :        Video_Type_Type)
   is
   begin
      S.Video_Type := New_Type;
      Save_Value
        (Reg, Reg_Key_Video_Emulation, Video_Type_Type'Pos (New_Type));
   end Set_Video_Type;

   ------------------------
   -- Set_Video_Standard --
   ------------------------

   procedure Set_Video_Standard
     (S            : in out Settings_Type; Reg : in out Registry_Type;
      New_Standard :        Video_Standard_Type)
   is
   begin
      S.Video_Standard := New_Standard;
      Save_Value
        (Reg, Reg_Key_Video_Standard, Video_Standard_Type'Pos (New_Standard));
   end Set_Video_Standard;

   --------------------------
   -- Set_Monochrome_Color --
   --------------------------

   procedure Set_Monochrome_Color
     (S         : in out Settings_Type; Reg : in out Registry_Type;
      New_Color :        SDL.Video.Palettes.RGB_Colour)
   is
      Color_String : String := "#xxxxxx";
   begin
      S.Mono_Color := New_Color;

      Put_Hex_Byte (Color_String (2 .. 3), Unsigned_8 (S.Mono_Color.Red));
      Put_Hex_Byte (Color_String (4 .. 5), Unsigned_8 (S.Mono_Color.Green));
      Put_Hex_Byte (Color_String (6 .. 7), Unsigned_8 (S.Mono_Color.Blue));
      Save_String (Reg, Reg_Key_Monochrome_Color, Color_String);
   end Set_Monochrome_Color;

   --------------------
   -- Set_Sound_Card --
   --------------------

   procedure Set_Sound_Card
     (S             : in out Settings_Type; Reg : in out Registry_Type;
      New_Card_Type :        Sound_Card_Type)
   is
   begin
      S.Sound_Card := New_Card_Type;
      Save_Value
        (Reg, Reg_Key_Sound_Card_Type, Sound_Card_Type'Pos (New_Card_Type));
   end Set_Sound_Card;

   -----------------------------
   -- Set_Disk_Image_Filename --
   -----------------------------

   procedure Set_Disk_Image_Filename
     (S     : in out Settings_Type; Reg : in out Registry_Type;
      Drive :        Disk_2_Range; New_Name : String)
   is
   begin
      Set_Unbounded_String (S.Disk_Image_Filenames (Drive), New_Name);
      Save_String
        (Reg,
         (if Drive = 1 then Reg_Key_Disk_Image_2 else Reg_Key_Disk_Image_1),
         New_Name);
   end Set_Disk_Image_Filename;

   --------------------------------
   -- Set_Keyboard_Rocker_Switch --
   --------------------------------

   procedure Set_Keyboard_Rocker_Switch
     (S          : in out Settings_Type; Reg : in out Registry_Type;
      New_Enable :        Boolean)
   is
   begin
      S.Keyboard_Rocker_Switch := New_Enable;
      Save_Boolean (Reg, Reg_Key_KB_Rocker_Switch, New_Enable);
   end Set_Keyboard_Rocker_Switch;

   -----------------------
   -- Set_Joystick_Type --
   -----------------------

   procedure Set_Joystick_Type
     (S      : in out Settings_Type; Reg : in out Registry_Type;
      Number :        Joystick_Range; New_Type : Joystick_Device)
   is
   begin
      S.Joystick_Types (Number) := New_Type;
      Save_Value
        (Reg, (if Number = 1 then Reg_Key_Joy_Type_1 else Reg_Key_Joy_Type_0),
         Joystick_Device'Pos (New_Type));
   end Set_Joystick_Type;

   -------------------------
   -- Set_SDL_Joystick_ID --
   -------------------------

   procedure Set_SDL_Joystick_ID
     (S      : in out Settings_Type; Reg : in out Registry_Type;
      Number :        Joystick_Range; New_ID : All_Devices)
   is
   begin
      S.SDL_Joystick_IDs (Number) := New_ID;
      Save_Value
        (Reg,
         (if Number = 1 then Reg_Key_Joy_1_Index else Reg_Key_Joy_0_Index),
         Unsigned_32 (New_ID));
   end Set_SDL_Joystick_ID;

   --------------------------
   -- Set_Printer_Filename --
   --------------------------

   procedure Set_Printer_Filename
     (S : in out Settings_Type; Reg : in out Registry_Type; New_Name : String)
   is
   begin
      Set_Unbounded_String (S.Printer_Filename, New_Name);
      Save_String (Reg, Reg_Key_Printer_Filename, New_Name);
   end Set_Printer_Filename;

   ------------------------------
   -- Set_Printer_Idle_Timeout --
   ------------------------------

   procedure Set_Printer_Idle_Timeout
     (S           : in out Settings_Type; Reg : in out Registry_Type;
      New_Timeout :        Unsigned_32)
   is
   begin
      S.Printer_Idle_Timeout := New_Timeout;
      Save_Value (Reg, Reg_Key_Printer_Idle_Limit, New_Timeout);
   end Set_Printer_Idle_Timeout;

   --------------------------------
   -- Set_Printer_Append_To_File --
   --------------------------------

   procedure Set_Printer_Append_To_File
     (S : in out Settings_Type; Reg : in out Registry_Type; Append : Boolean)
   is
   begin
      S.Printer_Append_To_File := Append;
      Save_Boolean (Reg, Reg_Key_Printer_Append, Append);
   end Set_Printer_Append_To_File;

end Apple2.Settings;
