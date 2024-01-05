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

with Ada.Characters.Latin_1;
with Ada.Directories;   use Ada.Directories;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Maps;
with Ada.Text_IO;       use Ada.Text_IO;

package body Emu.Registry with
  SPARK_Mode => Off
is
   use Reg_Hash_Map_Package;

   Backup_File_Extension : constant String := ".bak";
   --  Config files are renamed with this extension before being copied
   --  with modifications to the replacement file at the original location.
   --  We'll delete the original file on success, and if there are any I/O
   --  errors, we'll leave everything untouched, for debugging purposes.
   --
   --  Note that Ada.Directories doesn't support the usual POSIX trick of
   --  creating the updated file with a new name and then renaming it on top
   --  of the old file to replace it atomically.

   Tab : constant Character := Ada.Characters.Latin_1.HT;

   Whitespace : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (" " & Tab);
   --  The set of whitespace chars to trim consists of " " and horizontal tab

   procedure Import_Settings_Line
     (Registry : in out Registry_Type; Line : String; Index_Equals : Natural);
   --  Import a single settings line

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Registry : in out Registry_Type; File_Name : String)
   is
   begin
      Registry.File_Name     := new String'(File_Name);
      Registry.Request_Queue := new Request_Queues.Queue;

      --  Create the background write task and give it the filename and queue
      Registry.Update_Task :=
        new Update_Registry_File_Task
          (Registry.File_Name, Registry.Request_Queue);
   end Initialize;

   ---------------------------
   -- Hash for Reg_Key_Type --
   ---------------------------

   function Hash (Key : Reg_Key_Type) return Hash_Type is
   begin
      return Ada.Strings.Hash (Key);
   end Hash;

   --------------------------------------
   -- Equivalent_Keys for Reg_Key_Type --
   --------------------------------------

   function Equivalent_Keys (Left, Right : Reg_Key_Type) return Boolean is
   begin
      return String (Left) = String (Right);
   end Equivalent_Keys;

   ----------------------------
   -- "=" for Reg_Value_Type --
   ----------------------------

   overriding function "=" (Left, Right : Reg_Value_Type) return Boolean is
   begin
      if Left.Data_Type /= Right.Data_Type then
         return False;
      else
         case Left.Data_Type is
            when Type_String =>
               return Left.Value_String = Right.Value_String;
            when Type_Unsigned_32 =>
               return Left.Value_Unsigned_32 = Right.Value_Unsigned_32;
         end case;
      end if;
   end "=";

   ----------------
   -- Add_String --
   ----------------

   procedure Add_String
     (Registry : in out Registry_Type; Key : String; Value : String)
   is
      Existing_Key : constant Cursor := Find (Registry.Hash_Map, Key);

      New_Value : constant Reg_Value_Type :=
        (Type_String, To_Unbounded_String (Value));
   begin
      if Existing_Key /= No_Element then
         Replace_Element (Registry.Hash_Map, Existing_Key, New_Value);
         --  Note: silently replace any existing registry value
      else
         Insert (Registry.Hash_Map, Key, New_Value);
      end if;
   end Add_String;

   ---------------
   -- Add_Value --
   ---------------

   procedure Add_Value
     (Registry : in out Registry_Type; Key : String; Value : Unsigned_32)
   is
      Existing_Key : constant Cursor         := Find (Registry.Hash_Map, Key);
      New_Value    : constant Reg_Value_Type := (Type_Unsigned_32, Value);
   begin
      if Existing_Key /= No_Element then
         Replace_Element (Registry.Hash_Map, Existing_Key, New_Value);
         --  Note: silently replace any existing registry value
      else
         Insert (Registry.Hash_Map, Key, New_Value);
      end if;
   end Add_Value;

   -----------------
   -- Add_Boolean --
   -----------------

   procedure Add_Boolean
     (Registry : in out Registry_Type; Key : String; Value : Boolean)
   is
   begin
      Add_Value (Registry, Key, (if Value then 1 else 0));
   end Add_Boolean;

   --------------------------
   -- Import_Settings_Line --
   --------------------------

   procedure Import_Settings_Line
     (Registry : in out Registry_Type; Line : String; Index_Equals : Natural)
   is
      Reg_Key : constant String :=
        Trim (Line (Line'First .. Index_Equals - 1), Whitespace, Whitespace);

      Reg_Value : constant String :=
        Trim (Line (Index_Equals + 1 .. Line'Last), Whitespace, Whitespace);

      Existing_Key : constant Cursor := Find (Registry.Hash_Map, Reg_Key);
   begin
      if Reg_Key (1) = '#' then
         null;  --  skip comment lines
      elsif Existing_Key /= No_Element then
         declare
            New_Value : Reg_Value_Type := Element (Existing_Key);
         begin
            case New_Value.Data_Type is
               when Type_String =>
                  Set_Unbounded_String (New_Value.Value_String, Reg_Value);
               when Type_Unsigned_32 =>
                  begin
                     New_Value.Value_Unsigned_32 :=
                       Unsigned_32'Value (Reg_Value);
                  exception
                     when Constraint_Error =>
                        Put_Line
                          ("expected an integer for key: " & Reg_Key &
                           " value: " & Reg_Value);
                        New_Value.Value_Unsigned_32 := 0;
                  end;
            end case;
            Replace_Element (Registry.Hash_Map, Existing_Key, New_Value);
         end;
      else
         Put_Line ("unrecognized key: " & Reg_Key & " value: " & Reg_Value);
      end if;
   end Import_Settings_Line;

   -------------------
   -- Load_Settings --
   -------------------

   procedure Load_Settings
     (Registry : in out Registry_Type; Status : out File_Op_Status_Type)
   is
      File : File_Type;
   begin
      --  First, test if the file exists and return early if not
      if not Exists (Registry.File_Name.all) then
         Status := File_Not_Found;
         return;
      end if;

      --  Ada.Text_IO will throw an exception if we run into problems
      Open (File, In_File, Registry.File_Name.all);

      while not End_Of_File (File) loop
         --  read line by line, importing settings the app knows about
         declare
            Line         : constant String  := Get_Line (File);
            Index_Equals : constant Natural := Index (Line, "=", 1);
         begin
            if Index_Equals /= 0 then
               Import_Settings_Line (Registry, Line, Index_Equals);
            end if;
         end;
      end loop;

      Close (File);

   exception
      when others =>
         Put_Line ("Error reading config file: " & Registry.File_Name.all);
         Status := File_Error;
   end Load_Settings;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Registry : Registry_Type; Key : String) return String
   is
      Found_Key : constant Cursor := Find (Registry.Hash_Map, Key);
   begin
      if Found_Key /= No_Element then
         declare
            Found_Element : constant Reg_Value_Type := Element (Found_Key);
         begin
            if Found_Element.Data_Type = Type_String then
               return To_String (Found_Element.Value_String);
            else
               Put_Line
                 ("Error: Get_String found integer value for key: " & Key);
               return Found_Element.Value_Unsigned_32'Image;
               --  Print an error message and return value as a String
            end if;
         end;
      else
         Put_Line ("Error: Registry is missing key: " & Key);
         return "";
      end if;
   end Get_String;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Registry : Registry_Type; Key : String) return Unsigned_32
   is
      Found_Key : constant Cursor := Find (Registry.Hash_Map, Key);
   begin
      if Found_Key /= No_Element then
         declare
            Found_Element : constant Reg_Value_Type := Element (Found_Key);
         begin
            if Found_Element.Data_Type = Type_Unsigned_32 then
               return Found_Element.Value_Unsigned_32;
            else
               Put_Line
                 ("Error: Get_Value found string value for key: " & Key);
               return 0;
               --  Print an error message and return 0
            end if;
         end;
      else
         Put_Line ("Error: Registry is missing key: " & Key);
         return 0;
      end if;
   end Get_Value;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean (Registry : Registry_Type; Key : String) return Boolean
   is
   begin
      return (if Get_Value (Registry, Key) /= 0 then True else False);
   end Get_Boolean;

   -----------------
   -- Save_String --
   -----------------

   procedure Save_String
     (Registry : in out Registry_Type; Key : String; Value : String)
   is
      Existing_Key : constant Cursor := Find (Registry.Hash_Map, Key);

      New_Value : constant Reg_Value_Type :=
        (Type_String, To_Unbounded_String (Value));

      Value_Changed : Boolean := False;
   begin
      --  See if the value has been changed, and, if so, update config file
      --  If the found value is Unsigned_32 type, we replace with a String
      if Existing_Key /= No_Element then
         if Element (Existing_Key).Data_Type /= Type_String
           or else Element (Existing_Key).Value_String /= Value
         then
            Replace_Element (Registry.Hash_Map, Existing_Key, New_Value);
            Value_Changed := True;
         end if;
      else
         --  We shouldn't reach this path; the caller should have added
         --  a string with a default value. Handle it gracefully, anyway.
         Insert (Registry.Hash_Map, Key, New_Value);
         Value_Changed := True;
      end if;

      if Value_Changed then
         declare
            Request : constant Update_Task_Request :=
              (Command => Update_Key, Key => To_Unbounded_String (Key),
               Value   => To_Unbounded_String (Value));
         begin
            Registry.Request_Queue.Enqueue (Request);
         end;
      end if;
   end Save_String;

   ----------------
   -- Save_Value --
   ----------------

   procedure Save_Value
     (Registry : in out Registry_Type; Key : String; Value : Unsigned_32)
   is
      Existing_Key : constant Cursor := Find (Registry.Hash_Map, Key);

      New_Value : constant Reg_Value_Type := (Type_Unsigned_32, Value);

      Value_Changed : Boolean := False;
   begin
      --  See if the value has been changed, and, if so, update config file
      --  If the found value is String type, we replace with an Unsigned_32
      if Existing_Key /= No_Element then
         if Element (Existing_Key).Data_Type /= Type_Unsigned_32
           or else Element (Existing_Key).Value_Unsigned_32 /= Value
         then
            Replace_Element (Registry.Hash_Map, Existing_Key, New_Value);
            Value_Changed := True;
         end if;
      else
         --  We shouldn't reach this path; the caller should have added
         --  a string with a default value. Handle it gracefully, anyway.
         Insert (Registry.Hash_Map, Key, New_Value);
         Value_Changed := True;
      end if;

      if Value_Changed then
         declare
            Request : constant Update_Task_Request :=
              (Command => Update_Key, Key => To_Unbounded_String (Key),
               Value   => To_Unbounded_String (Value'Image));
         begin
            Registry.Request_Queue.Enqueue (Request);
         end;
      end if;
   end Save_Value;

   ------------------
   -- Save_Boolean --
   ------------------

   procedure Save_Boolean
     (Registry : in out Registry_Type; Key : String; Value : Boolean)
   is
   begin
      Save_Value (Registry, Key, (if Value then 1 else 0));
   end Save_Boolean;

   -----------------------------
   -- End_Registry_Write_Task --
   -----------------------------

   --  This will leak the filename string, queue, and task objects, but the
   --  program should be exiting soon after calling this.

   procedure End_Registry_Write_Task (Registry : in out Registry_Type) is
      Request : constant Update_Task_Request :=
        (Command => Exit_Task, Key => Null_Unbounded_String,
         Value   => Null_Unbounded_String);
   begin
      Registry.Request_Queue.Enqueue (Request);
   end End_Registry_Write_Task;

   -------------------------------
   -- Update_Registry_File_Task --
   -------------------------------

   task body Update_Registry_File_Task is
      Requests : array (1 .. 10) of Update_Task_Request := (others => <>);

      Num_Requests : Natural := 0;

      Received_Exit_Task : Boolean := False;

      Backup_Name : constant String := File_Name.all & Backup_File_Extension;

      procedure Replace_Settings_Line
        (Line : String; Index_Equals : Natural; File : File_Type);
      --  Replace a single settings line if any keys match updates

      procedure Replace_Settings_Line
        (Line : String; Index_Equals : Natural; File : File_Type)
      is
         Old_Key : constant String :=
           Trim
             (Line (Line'First .. Index_Equals - 1), Whitespace, Whitespace);
      begin
         for I in 1 .. Num_Requests loop
            if Requests (I).Key = Old_Key then
               Put_Line
                 (File,
                  Tab & To_String (Requests (I).Key) & " =" & Tab &
                  To_String (Requests (I).Value));

               Requests (I .. Num_Requests - 1) :=
                 Requests (I + 1 .. Num_Requests);

               Num_Requests := Num_Requests - 1;
               return;
            end if;
         end loop;
         --  If we didn't match anything, print the original line unchanged
         Put_Line (File, Line);
      end Replace_Settings_Line;

   begin
      while not Received_Exit_Task loop
         --  Wait until we get some requests (or the process terminates)
         loop
            Queue.Dequeue (Requests (Num_Requests + 1));

            --  Process everything before exit task, then exit the loop
            if Requests (Num_Requests + 1).Command = Exit_Task then
               Received_Exit_Task := True;
            else
               Num_Requests := Num_Requests + 1;
            end if;

            exit when Queue.Current_Use = 0 or Num_Requests = Requests'Last;
         end loop;

         declare
            New_File, Old_File : File_Type;
         begin
            --  If there's an existing file, rename it and copy its lines
            if Exists (File_Name.all) then
               begin
                  if Exists (Backup_Name) then
                     Delete_File (Backup_Name);
                  end if;
                  Rename (File_Name.all, Backup_Name);
               exception
                  when others =>
                     goto Bottom_Of_Loop;
               end;

               Open (Old_File, In_File, Backup_Name);
               Create (New_File, Out_File, File_Name.all);

               while not End_Of_File (Old_File) loop
                  --  copy lines from original file, replacing changed lines
                  declare
                     Line         : constant String  := Get_Line (Old_File);
                     Index_Equals : constant Natural := Index (Line, "=", 1);
                  begin
                     if Index_Equals /= 0 then
                        Replace_Settings_Line (Line, Index_Equals, New_File);
                     else
                        Put_Line (New_File, Line);
                     end if;
                  end;
               end loop;

               Close (Old_File);
               Delete_File (Backup_Name);
               --  Finished with old file; add any new config items below
            else
               --  Handle the simpler case of creating a new config file
               Create (New_File, Out_File, File_Name.all);
            end if;

            --  Write any unhandled updated lines to new config file
            for I in 1 .. Num_Requests loop
               Put_Line
                 (New_File,
                  Tab & To_String (Requests (I).Key) & " =" & Tab &
                  To_String (Requests (I).Value));
            end loop;

            Close (New_File);
         exception
            when others =>
               Put_Line ("Error updating config file " & File_Name.all);
         end;

         --  Reset Num_Requests and loop to wait for more requests
         Num_Requests := 0;

         --  When we receive Exit_Task, we go here to exit the loop
         <<Bottom_Of_Loop>>
      end loop;
   end Update_Registry_File_Task;

end Emu.Registry;
