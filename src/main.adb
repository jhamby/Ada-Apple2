--  Temporary Main procedure for initial testing

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO;

with Apple2;        use Apple2;
with Apple2.Memory; use Apple2.Memory;

procedure Main is
   procedure Print_Type;

   procedure Print_Type is
   begin
      if Is_Apple2 then
         Ada.Text_IO.Put_Line ("original Apple II");
      else
         Ada.Text_IO.Put_Line ("Apple IIe or IIe Enhanced");
      end if;
   end Print_Type;

   Mem_Range_1   : Mem_Range_Access (16#2000# .. 16#4FFF#) := null;
   Mem_Range_2   : Mem_Range_Access (16#4000# .. 16#6FFF#) := null;
   IO_Read_Value : Value_8_Bit                             := 0;
begin
   --  Default type should be Apple IIe Enhanced
   Print_Type;
   Assert (not Is_Apple2);

   Set_Machine_Type (Apple_2_Plus);
   Print_Type;
   Assert (Is_Apple2);

   Set_Machine_Type (Apple_2e);
   Print_Type;
   Assert (not Is_Apple2);

   Init_IO_Handlers;
   Mem_Initialize;
   Mem_Get_Main_Range (16#2000#, 16#3000#, Mem_Range_1);
   Mem_Get_Aux_Range (16#4000#, 16#3000#, Mem_Range_2);
   for I in Mem_Range_1'First .. Mem_Range_1'Last loop
      Mem_Range_2.all (I + 16#2000#) := Mem_Range_1.all (I);
   end loop;

   for I in Address_16_Bit (16#C000#) .. Address_16_Bit (16#CFFF#) loop
      Mem_IO_Read (I, I, IO_Read_Value, 42);
      Mem_IO_Write (I, I, IO_Read_Value, 64);
   end loop;
   if IO_Read_Value /= 0 then
      Ada.Text_IO.Put_Line ("I/O read value /= 0");
   end if;
end Main;
