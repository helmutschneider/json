with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;

package body Json is
    subtype I32 is Integer;
    subtype I64 is Long_Long_Integer;
    subtype F32 is Float;
    subtype F64 is Long_Float;
    subtype Str is Unbounded_String;
    subtype Char is Character;
    subtype Index is Natural;
    type Parser is record
        Data : Ada.Strings.Unbounded.Unbounded_String;
        Cursor : Index;
   end record;

    function Length(S : Str) return I32 is
    begin
       return Ada.Strings.Unbounded.Count(S, ISO_646_Set);
    end;

    function Char_At(S : Str; I : Index) return Char is
    begin
        return Ada.Strings.Unbounded.Element(S, I + 1);
    end;

    function "=" (Left : JsonNode; Right : JsonNode) return Boolean is
    begin
        return Left.Kind = Right.Kind;
    end;

    function Parse(S : String) return JsonNode is
    begin
        return Json.Parse(To_Unbounded_String(S));
    end;

    function Parse(S : Str) return JsonNode is
        Result : JsonNode;
        Ch : Char;
    begin
        Put_Line("Cowabunga!");
        Result := (Kind => JsonStr, Value => To_Unbounded_String("yee!"));

        for I in 0 .. Length(S) - 1 loop
            Ch := Char_At(S, I);
            Put_Line(Ch'Image);
        end loop;

        return Result;
    end;
end;
