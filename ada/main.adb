with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Assertions;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Assertions;             use Ada.Assertions;
with Json;                       use Json;

procedure Main is
   type TestCase is record
      Expected : JsonNode;
      Value    : Unbounded_String;
   end record;

   procedure Run_Tests is
      Actual : JsonNode;
      Cases  : constant array (Positive range <>) of TestCase :=
        (1 =>
           (Expected => (Kind => JsonNull),
            Value    => To_Unbounded_String ("null")),
         2 =>
           (Expected => (Kind => JsonNull),
            Value    => To_Unbounded_String ("n")),
         3 =>
           (Expected =>
              (Kind => JsonString, Str => To_Unbounded_String ("cowabunga!")),
            Value    => To_Unbounded_String ("""cowabunga!""")),
         4 =>
           (Expected =>
              (Kind => JsonString, Str => To_Unbounded_String ("cowa\bunga!")),
            Value    => To_Unbounded_String ("""cowa\\bunga!""")),
         5 =>
           (Expected => (Kind => JsonBoolean, Bool => True),
            Value    => To_Unbounded_String ("true")),
         6 =>
           (Expected => (Kind => JsonBoolean, Bool => True),
            Value    => To_Unbounded_String ("t")),
         7 =>
           (Expected => (Kind => JsonBoolean, Bool => False),
            Value    => To_Unbounded_String ("false")),
         8 =>
           (Expected => (Kind => JsonBoolean, Bool => False),
            Value    => To_Unbounded_String ("f")),
         9 =>
           (Expected => (Kind => JsonNumber, Num => 1.5),
            Value    => To_Unbounded_String ("1.5")));
   begin
      for K in 1 .. Cases'Length loop
         Actual := Json.Parse (Cases (K).Value);
         Assert (Equal (Cases (K).Expected, Actual));
      end loop;

      -- array tests
      declare
         Actual   : JsonNode;
         Expected : JsonNode;
         Items    : JsonVectors.Vector;
         Node     : JsonNodeAccess;
      begin
         Node := new JsonNode'(Kind => JsonBoolean, Bool => True);
         Items.Append (Node);
         Node := new JsonNode'(Kind => JsonNull);
         Items.Append (Node);
         Node := new JsonNode'(Kind => JsonBoolean, Bool => False);
         Items.Append (Node);
         Node := new JsonNode'(Kind => JsonNumber, Num => 3.2);
         Items.Append (Node);
         Expected := (Kind => JsonArray, Items => Items);
         Actual   := Json.Parse ("[true, null, false, 3.2]");
         Assert (Equal (Expected, Actual));
      end;
      Put_Line ("Ok!");
   end Run_Tests;

begin
   Run_Tests;
end Main;
