with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Json;                  use Json;
with Tests;

procedure Main is
   F      : File_Type;
   Str    : Unbounded_String;
   Parsed : JsonNode;
begin
   Tests.Run;

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("Usage:");
      Put_Line ("  ./app [path_to_json_file]");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   Ada.Text_IO.Open (F, In_File, Ada.Command_Line.Argument (1));
   while not Ada.Text_IO.End_Of_File (F) loop
      Str := Str & Ada.Text_IO.Get_Line (F);
   end loop;
   Ada.Text_IO.Close (F);
   Parsed := Json.Parse (Str);
   Put_Line (To_String (Parsed));
end Main;
