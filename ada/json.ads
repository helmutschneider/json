with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Json is
    type JsonNode;
    type JsonNodeAccess is access JsonNode;
    package JsonVectors is new Ada.Containers.Vectors(
        Index_Type => Positive,
        Element_Type => JsonNodeAccess
    );
    type JsonKind is (JsonNull, JsonBoolean, JsonNumber, JsonString, JsonArray);
    type JsonNode (Kind : JsonKind := JsonNull) is record
        case Kind is
            when JsonNull =>
                null;
            when JsonBoolean =>
                Bool : Boolean;
            when JsonNumber =>
                Num : Long_Float;
            when JsonString =>
                Str : Unbounded_String;
            when JsonArray =>
                Items : JsonVectors.Vector;
        end case;
    end record;
   
   function Parse(S : String) return JsonNode;
   function Parse(S : Unbounded_String) return JsonNode;
   function Equal(Left : JsonNode; Right : JsonNode) return Boolean;
end;
