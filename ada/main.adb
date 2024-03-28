with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Assertions; use Ada.Assertions;
with Json; use Json;

procedure Main is
   procedure Run_Tests is
      Actual : JsonNode;
      Expected : JsonNode;
   begin
      begin
         Actual := Json.Parse("""cowabunga!""");
         Expected := (Kind => JsonStr, Str => To_Unbounded_String("cowabunga!"));
         Assert(Equal(Expected, Actual));
      end;
      begin
         Actual := Json.Parse("""esc\\aped!""");
         Expected := (Kind => JsonStr, Str => To_Unbounded_String("esc\aped!"));
         Assert(Equal(Expected, Actual));
      end;
   end;

   MY_JSON_STR : String := "{ ""yee"": [1, 2, 3] }";
   X : Unbounded_String := To_Unbounded_String(MY_JSON_STR);
   N : JsonNode := (Kind => JsonNull);
begin
   Run_Tests;

   N := Json.Parse(X);
end Main;
