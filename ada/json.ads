with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Json is
    type JsonKind is (JsonNull, JsonStr, JsonBool);
    type JsonNode (Kind : JsonKind := JsonNull) is record
        case Kind is
            when JsonNull =>
                null;
            when JsonStr =>
                Str : Unbounded_String;
            when JsonBool =>
                Bool : Boolean;
        end case;
    end record;
   
   function Parse(S : String) return JsonNode;
   function Parse(S : Unbounded_String) return JsonNode;
   function Equal(Left : JsonNode; Right : JsonNode) return Boolean;
end;
