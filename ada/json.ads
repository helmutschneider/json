with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Json is
    type JsonKind is (JsonNull, JsonStr);
    type JsonNode (Kind : JsonKind := JsonNull) is record
        case Kind is
            when JsonNull =>
                null;
            when JsonStr =>
                Value : Ada.Strings.Unbounded.Unbounded_String;
        end case;
    end record;
   
   function Parse(S : String) return JsonNode;
   function Parse(S : Ada.Strings.Unbounded.Unbounded_String) return JsonNode;
end;
