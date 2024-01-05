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

----------------------------------------------------------------------------
--  This package handles I/O for config files with "key = value" lines and
--  comment lines starting with "#". Leading and trailing spaces and tabs
--  are trimmed from both sides of the "=" sign. When the user changes any
--  setting, the program's settings package must call either Save_String,
--  Save_Value, or Save_Boolean to update the value in memory and on disk.
--  Each registry object has a background task to update the config file.
--
--  A setting is only persisted if it's not equal to the previous value.
--  Changing a setting away from and then back to the default value will
--  cause that key to be written to the config file. There's no code to
--  rewrite a config file to delete a previously-added line, so in this case,
--  leaving a not-strictly-necessary additional line in the user's config
--  file is a small price to pay for relative simplicity.
--
--  Command-line options override the Settings object in memory, but aren't
--  written to the Registry object, nor saved to the config file. If the
--  user changes a setting to a value different to the one in the Registry,
--  then it will be written to the config file. This means that no lines
--  will be added to the config file if the user sets a non-default setting
--  on the command-line and then changes it to the default or saved value.
----------------------------------------------------------------------------

with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Emu.Registry with
  SPARK_Mode => Off
is
   type Registry_Type is private;
   --  Private type containing registry filename and hash map

   procedure Initialize (Registry : in out Registry_Type; File_Name : String);
   --  Initialize the registry with the specified file pathname

   procedure Add_String
     (Registry : in out Registry_Type; Key : String; Value : String);
   --  Add a new string value to the registry, with default value

   procedure Add_Value
     (Registry : in out Registry_Type; Key : String; Value : Unsigned_32);
   --  Add a new unsigned integer value to the registry, with default value

   procedure Add_Boolean
     (Registry : in out Registry_Type; Key : String; Value : Boolean);
   --  Add a new boolean value to the registry, with default value

   procedure Load_Settings
     (Registry : in out Registry_Type; Status : out File_Op_Status_Type);
   --  Load settings from file and report status. Call this after adding
   --  all of the registry keys and default values that you care about.

   function Get_String (Registry : Registry_Type; Key : String) return String;
   --  Get a string value from the registry, whether loaded from config file
   --  or a default value added previously with Add_String.

   function Get_Value
     (Registry : Registry_Type; Key : String) return Unsigned_32;
   --  Get an unsigned int value from the registry, whether loaded from config
   --  file or a default value added previously with Add_Value.

   function Get_Boolean
     (Registry : Registry_Type; Key : String) return Boolean;
   --  Get a boolean value from the registry, whether loaded from config
   --  file or a default value added previously with Add_Boolean.

   procedure Save_String
     (Registry : in out Registry_Type; Key : String; Value : String);
   --  Save a String to the registry as a background task

   procedure Save_Value
     (Registry : in out Registry_Type; Key : String; Value : Unsigned_32);
   --  Save an Unsigned_32 to the registry as a background task

   procedure Save_Boolean
     (Registry : in out Registry_Type; Key : String; Value : Boolean);
   --  Save a Boolean to the registry as a background task

   procedure End_Registry_Write_Task (Registry : in out Registry_Type);
   --  Call to ensure the registry write finishes before we exit the program

private
   type Reg_Value_Data_Type is (Type_String, Type_Unsigned_32);
   --  Enum for the two data types we support: string and Unsigned_32

   type Reg_Value_Type (Data_Type : Reg_Value_Data_Type) is record
      case Data_Type is
         when Type_String =>
            Value_String : Unbounded_String;
         when Type_Unsigned_32 =>
            Value_Unsigned_32 : Unsigned_32;
      end case;
   end record;
   --  Registry value type with variant part for the supported types

   subtype Reg_Key_Type is String;
   --  Subtype for registry key

   function Hash (Key : Reg_Key_Type) return Hash_Type with
     Inline;
   --  Hash function for generic Hashed_Maps package

   function Equivalent_Keys (Left, Right : Reg_Key_Type) return Boolean with
     Inline;
   --  Use built-in String compare for key comparison

   overriding function "=" (Left, Right : Reg_Value_Type) return Boolean with
     Inline;
   --  Equality function for registry values

   --  Note: Indefinite_Hashed_Maps support unconstrained key and element
   --  types such as String and Reg_Value_Type, while Hashed_Maps require
   --  constrained types such as Unbounded_String.

   package Reg_Hash_Map_Package is new Ada.Containers.Indefinite_Hashed_Maps
     (Reg_Key_Type, Reg_Value_Type, Hash, Equivalent_Keys, "=");
   --  Hash map for registry keys and their values

   type Update_Task_Command is (Update_Key, Exit_Task);
   --  The commands recognized by Update_Registry_File_Task

   type Update_Task_Request is record
      Command    : Update_Task_Command := Exit_Task;
      Key, Value : Unbounded_String    := Null_Unbounded_String;
   end record;
   --  Note: using Unbounded_String makes the record a fixed size

   package Request_Queue_Interfaces is new Ada.Containers
     .Synchronized_Queue_Interfaces
     (Update_Task_Request);
   --  Create a queue interface package for our request type

   package Request_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Request_Queue_Interfaces);
   --  Create the synchronized queue package for our request type

   type Access_Request_Queue is access Request_Queues.Queue;

   task type Update_Registry_File_Task
     (File_Name : String_Access; Queue : Access_Request_Queue);
   --  We'll create this task along with the registry object

   type Access_Update_Task is access Update_Registry_File_Task;

   type Registry_Type is record
      File_Name : String_Access;
      --  Pathname to the registry config file

      Hash_Map : Reg_Hash_Map_Package.Map;
      --  Hash map for keys and values

      Request_Queue : Access_Request_Queue := null;
      --  Thread-safe queue to send write requests to worker task

      Update_Task : Access_Update_Task := null;
      --  Worker to write updated registry values to disk until exited
   end record;
   --  Record type for program registry entries and save file info

end Emu.Registry;
