with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Json is
    type JsonNode;
    type JsonNodeAccess is access JsonNode;
    package JsonVectors is new Ada.Containers.Vectors(
        Index_Type => Positive,
        Element_Type => JsonNodeAccess
    );
    package JsonMaps is new Ada.Containers.Indefinite_Hashed_Maps(
        Key_Type => String,
        Element_Type => JsonNodeAccess,
        Hash => Ada.Strings.Hash,
        Equivalent_Keys => "="
    );
    type JsonKind is (JsonNull, JsonBoolean, JsonNumber, JsonString, JsonArray, JsonObject);
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
                Vec : JsonVectors.Vector;
            when JsonObject =>
                Map : JsonMaps.Map;
        end case;
    end record;
   
   function Parse(Str : String) return JsonNode;
   function Parse(Str : Unbounded_String) return JsonNode;
   function Equal(Left : JsonNode; Right : JsonNode) return Boolean;
   function To_String(Node : JsonNode) return String;
end;
