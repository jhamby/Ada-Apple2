--  Temporary Main procedure for initial testing

with Ada.Text_IO;

with Apple2; use Apple2;

with Apple2.Memory; use Apple2.Memory;

with Emu; use Emu;

with Emu.Memory; use Emu.Memory;

with Interfaces; use Interfaces;

procedure Main is

   type Global_RAM_Access is access RAM_All_Banks;

   Main_Mem : constant Global_RAM_Access := new RAM_All_Banks;

   procedure Run_Tests (C : in out Computer; Mem : access RAM_All_Banks);
   --  Exercise the computer object

   procedure Run_Tests (C : in out Computer; Mem : access RAM_All_Banks) is
      pragma Unreferenced (Mem);
      IO_Read_Value : Unsigned_8;
   begin
      for I in Unsigned_16 (16#C000#) .. Unsigned_16 (16#CFFF#) loop
         Mem_IO_Read (C, Main_Mem, I, IO_Read_Value, 3);
         Mem_IO_Write (C, Main_Mem, I, 123, 3);
      end loop;

      CPU_Execute (C, Main_Mem, 10_000);

      if IO_Read_Value /= 0 then
         Ada.Text_IO.Put_Line ("I/O read value /= 0");
      end if;
   end Run_Tests;

begin

   --  Simulate instantiating a single machine based on startup config
   for I in 0 .. 3 loop
      case I is
         when 0 =>
            declare
               C : Computer;
            begin
               Init_Apple2 (C, Main_Mem, Apple_2);
               Run_Tests (C, Main_Mem);
            end;

         when 1 =>
            declare
               C : Computer;
            begin
               Init_Apple2 (C, Main_Mem, Apple_2_Plus);
               Run_Tests (C, Main_Mem);
            end;

         when 2 =>
            declare
               C : Computer;
            begin
               Init_Apple2 (C, Main_Mem, Apple_2e);
               Run_Tests (C, Main_Mem);
            end;

         when 3 =>
            declare
               C : Computer;
            begin
               Init_Apple2 (C, Main_Mem, Apple_2e_Enhanced);
               Run_Tests (C, Main_Mem);
            end;
      end case;
   end loop;

end Main;
