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

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;   use Ada.Text_IO;

with GNAT.Formatted_String; use GNAT.Formatted_String;

with WDC_CPU_65C02; use WDC_CPU_65C02;

package body Apple2.Benchmark with
  SPARK_Mode => Off
is
   Short_Opcodes : constant := 22;
   --  number of opcodes with zero-page, immediate, or no operand

   Total_Opcodes : constant := 33;
   --  size of the list of benchmark opcodes

   Bench_Opcodes : constant Mem_Byte_Range (0 .. Total_Opcodes - 1) :=
     (16#06#, 16#16#, 16#24#, 16#45#, 16#48#, 16#65#, 16#68#, 16#76#, 16#84#,
      16#85#, 16#86#, 16#91#, 16#94#, 16#A4#, 16#A5#, 16#A6#, 16#B1#, 16#B4#,
      16#C0#, 16#C4#, 16#C5#, 16#E6#, 16#19#, 16#6D#, 16#8D#, 16#99#, 16#9D#,
      16#AD#, 16#B9#, 16#BD#, 16#DD#, 16#ED#, 16#EE#);
   --  Opcodes for CPU_Setup_Benchmark to copy with additions

   -------------------------
   -- CPU_Setup_Benchmark --
   -------------------------

   procedure CPU_Setup_Benchmark
     (C : in out Computer; Mem : not null access RAM_All_Banks)
   is
      Address : Unsigned_16 := 16#0300#;
   begin
      C.A  := 0;
      C.X  := 0;
      C.Y  := 0;
      C.PC := 16#0300#;
      C.SP := 16#FF#;

      for I in Bench_Opcodes'First .. Bench_Opcodes'Last loop
         Mem_IO_Write (C, Mem, Address, Bench_Opcodes (I));
         Mem_IO_Write (C, Mem, Address + 1, Bench_Opcodes (I));
         Address := Address + 2;

         if I >= Short_Opcodes then
            Mem_IO_Write (C, Mem, Address, 16#00#);
            Address := Address + 1;
         end if;

         --  jump to the next sequence or back to the start of the code

         if I = Bench_Opcodes'Last or ((Address and 16#0F#) >= 16#0B#) then
            Mem_IO_Write (C, Mem, Address, 16#4C#);

            Mem_IO_Write
              (C, Mem, Address + 1,
               (if I = Bench_Opcodes'Last then 16#00#
                else Unsigned_8 (Address and 16#F0#) + 16#10#));

            Mem_IO_Write (C, Mem, Address + 2, 16#03#);
            Address := Address + 3;
            while (Address and 16#0F#) /= 0 loop
               Address := Address + 1;
            end loop;
         end if;
      end loop;
   end CPU_Setup_Benchmark;

   -----------------------
   -- CPU_Run_Benchmark --
   -----------------------

   Bench_Time : constant := 5;  --  seconds to run benchmark

   procedure CPU_Run_Benchmark
     (C : in out Computer; Mem : not null access RAM_All_Banks)
   is
      Lines_Per_Frame : constant Unsigned_16 :=
        (if C.Settings.Video_Standard = PAL then PAL_Lines_Per_Frame
         else NTSC_Lines_Per_Frame);
      Stop_Time       : constant Time        := Clock + Seconds (Bench_Time);

      Divisor : constant Float :=
        Float
          ((if C.Settings.Video_Standard = PAL then PAL_M14 else NTSC_M14) *
           Bench_Time) /
        14.0;
   begin
      --  Initialize counters to start of VBL
      C.Scan_Line      := Visible_Lines_Per_Frame;
      C.Column_Cycle   := 0;
      C.Starting_Cycle := Visible_Lines_Per_Frame * Cycles_Per_Line;

      if C.Settings.Model = Apple_2e_Enhanced then
         --  65C02 version
         while Clock < Stop_Time loop
            --  emulate CPU for the non-visible VBL + vertical borders
            CPU_Execute_WDC_65C02
              (CPU_6502_Series (C), Mem, Cycles_Per_Line,
               Lines_Per_Frame - Visible_Lines_Per_Frame);

            if C.PC < 16#0300# or C.PC > 16#03FF# then
               Benchmark_Error (C.PC, C.Cycles_Since_Boot - C.Starting_Cycle);
               return;
            end if;

            C.Scan_Line := 0;

            --  emulate CPU for each visible scan line
            for Line in 0 .. Visible_Lines_Per_Frame - 1 loop
               CPU_Execute_WDC_65C02
                 (CPU_6502_Series (C), Mem, Cycles_Per_Line, 1);

               --  this will be the video emulation in the real emulator
               if C.PC < 16#0300# or C.PC > 16#03FF# then
                  Benchmark_Error
                    (C.PC, C.Cycles_Since_Boot - C.Starting_Cycle);
                  return;
               end if;
            end loop;

            C.Frames_Since_Boot := C.Frames_Since_Boot + 1;
         end loop;
      else
         --  NMOS 6502 version
         while Clock < Stop_Time loop
            --  emulate CPU for the non-visible VBL + vertical borders
            CPU_Execute_MOS_6502
              (CPU_6502_Series (C), Mem, Cycles_Per_Line,
               Lines_Per_Frame - Visible_Lines_Per_Frame);

            if C.PC < 16#0300# or C.PC > 16#03FF# then
               Benchmark_Error (C.PC, C.Cycles_Since_Boot - C.Starting_Cycle);
               return;
            end if;

            C.Scan_Line := 0;

            --  emulate CPU for each visible scan line
            for Line in 0 .. Visible_Lines_Per_Frame - 1 loop
               CPU_Execute_MOS_6502
                 (CPU_6502_Series (C), Mem, Cycles_Per_Line, 1);

               --  this will be the video emulation in the real emulator
               if C.PC < 16#0300# or C.PC > 16#03FF# then
                  Benchmark_Error
                    (C.PC, C.Cycles_Since_Boot - C.Starting_Cycle);
                  return;
               end if;
            end loop;

            C.Frames_Since_Boot := C.Frames_Since_Boot + 1;
         end loop;
      end if;

      Put_Line
        (-
         (+"Emulated %d frames of %s @ %5.2f MHz" &
          Long_Integer (C.Frames_Since_Boot) &
          (if C.Settings.Video_Standard = PAL then "PAL" else "NTSC") &
          Float (C.Cycles_Since_Boot - C.Starting_Cycle) / Divisor));

      Put_Line
        (-
         (+"%s (%s), %d cycles" & C.Settings.Model'Image &
          (if C.Settings.Model = Apple_2e_Enhanced then "65C02" else "6502") &
          Long_Integer (C.Cycles_Since_Boot - C.Starting_Cycle)));
   end CPU_Run_Benchmark;

   ---------------------
   -- Benchmark_Error --
   ---------------------

   procedure Benchmark_Error (PC : Unsigned_16; Cycle : CPU_Cycle_Count) is
      PC_Hex : String (1 .. 4) := (others => ' ');
   begin
      Put_Line ("Error: unexpected PC while running the CPU benchmark.");
      Put_Hex_Byte (PC_Hex (1 .. 2), Unsigned_8 (Shift_Right (PC, 8)));
      Put_Hex_Byte (PC_Hex (3 .. 4), Unsigned_8 (PC and 16#FF#));
      Put_Line ("PC: $" & PC_Hex & " cycles: " & Cycle'Image);
   end Benchmark_Error;

end Apple2.Benchmark;
