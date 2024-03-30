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
           (Expected =>
              (Kind => JsonString, Str => To_Unbounded_String ("cowabunga!")),
            Value    => To_Unbounded_String ("""cowabunga!""")),
         3 =>
           (Expected =>
              (Kind => JsonString, Str => To_Unbounded_String ("cowa\bunga!")),
            Value    => To_Unbounded_String ("""cowa\\bunga!""")),
         4 =>
           (Expected => (Kind => JsonBoolean, Bool => True),
            Value    => To_Unbounded_String ("true")),
         5 =>
           (Expected => (Kind => JsonBoolean, Bool => False),
            Value    => To_Unbounded_String ("false")),
         6 =>
           (Expected => (Kind => JsonNumber, Num => 1.5),
            Value    => To_Unbounded_String ("1.5")),
         7 =>
           (Expected => (Kind => JsonNumber, Num => 420.0),
            Value    => To_Unbounded_String ("  420  ")));
   begin
      for K in 1 .. Cases'Length loop
         Actual := Json.Parse (Cases (K).Value);
         Assert (Equal (Cases (K).Expected, Actual));
      end loop;

      -- array with stuff
      declare
         Actual   : JsonNode;
         Expected : JsonNode;
         Vec    : JsonVectors.Vector;
         Node     : JsonNodeAccess;
      begin
         Node := new JsonNode'(Kind => JsonBoolean, Bool => True);
         Vec.Append (Node);
         Node := new JsonNode'(Kind => JsonNull);
         Vec.Append (Node);
         Node := new JsonNode'(Kind => JsonBoolean, Bool => False);
         Vec.Append (Node);
         Node := new JsonNode'(Kind => JsonNumber, Num => 3.2);
         Vec.Append (Node);
         Expected := (Kind => JsonArray, Vec => Vec);
         Actual   := Json.Parse ("[true, null, false, 3.2]");
         Assert (Equal (Expected, Actual));
      end;

      -- empty array
      declare
         Actual   : JsonNode;
         Expected : JsonNode;
         Vec    : JsonVectors.Vector;
      begin
         Expected := (Kind => JsonArray, Vec => Vec);
         Actual   := Json.Parse ("  [ ]  ");
         Assert (Equal (Expected, Actual));
      end;

      -- map with stuff
      declare
         Actual   : JsonNode;
         Expected : JsonNode;
         Map    : JsonMaps.Map;
         Node     : JsonNodeAccess;
      begin
         Node := new JsonNode'(Kind => JsonBoolean, Bool => True);
         Map.Include("yee", Node);
         Node := new JsonNode'(Kind => JsonNumber, Num => 42069.0);
         Map.Include("boi", Node);
         Expected := (Kind => JsonObject, Map => Map);
         Actual   := Json.Parse ("{ ""boi"" : 42069, ""yee"" : true }");
         Assert (Equal (Expected, Actual));
      end;

      -- empty map
      declare
         Actual   : JsonNode;
         Expected : JsonNode;
         Map    : JsonMaps.Map;
      begin
         Expected := (Kind => JsonObject, Map => Map);
         Actual   := Json.Parse ("{}");
         Assert (Equal (Expected, Actual));
      end;

      -- random parser tests
      declare
         S : constant array(Positive range<>) of Unbounded_String := (
            1 => To_Unbounded_String("{ ""bunga"": [1, 2, 3], ""boi"": { ""x"": 5 } }"),
            2 => To_Unbounded_String("  [ 420, {}, false     ]                  "),
            3 => To_Unbounded_String("[[[420, 69]]]"),
            4 => To_Unbounded_String("""hello!""")
         );
         Node : JsonNode;
      begin
         for X of S loop
            Node := Json.Parse(X);
         end loop;
      end;

      Put_Line ("Ok!");
   end Run_Tests;

begin
   Run_Tests;
end Main;
