with Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Json;                  use Json;
with Ada.Directories;
with Ada.Direct_IO;

procedure Main is
   function Read_File(Name : String) return String is
      Size : constant Ada.Directories.File_Size := Ada.Directories.Size(Name);
      subtype File_Str is String(1 .. Integer(Size));
      package String_IO is new Ada.Direct_IO(File_Str);
      Contents : File_Str;
      File : String_IO.File_Type;
   begin
      String_IO.Open(File, String_IO.In_File, Name);
      String_IO.Read(File, Contents);
      String_IO.Close(File);
      return Contents;
   end;

   Str    : Unbounded_String;
   Parsed : JsonNode;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("Usage:");
      Ada.Text_IO.Put_Line ("  ./app [path_to_json_file]");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   Str := Str & Read_File(Ada.Command_Line.Argument(1));
   Parsed := Json.Parse (Str);
   Ada.Text_IO.Put_Line (To_String (Parsed));
end Main;
