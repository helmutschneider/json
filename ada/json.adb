with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

package body Json is
    subtype I32 is Integer;
    subtype I64 is Long_Long_Integer;
    subtype F32 is Float;
    subtype F64 is Long_Float;
    type Parser is record
        Data : Unbounded_String;
        Length : I32;
        Index : I32;
    end record;

    function Length(S : Unbounded_String) return I32 is
    begin
       return Ada.Strings.Unbounded.Count(S, ISO_646_Set);
    end;

    function Char_At(S : Unbounded_String; I : I32) return Character is
    begin
        return Ada.Strings.Unbounded.Element(S, I + 1);
    end;

    function Is_Whitespace(Ch : Character) return Boolean is
    begin
        return Ch = ' '
            or Ch = ASCII.LF  -- \n
            or Ch = ASCII.CR  -- \r
            or Ch = ASCII.HT; -- \t
    end;

    function Equal(Left : JsonNode; Right : JsonNode) return Boolean is
    begin
        if Left.Kind /= Right.Kind then
            return False;
        end if;
        return (case Left.Kind is
            when JsonStr =>
                Left.Str = Right.Str,
            when JsonBool =>
                Left.Bool = Right.Bool,
            when others => True
        );
    end;

    function Parser_Peek(Parzer : Parser; Offset : I32) return Character is
    begin
        return Char_At(Parzer.Data, Parzer.Index + Offset);
    end;

    function Parser_Token(Parzer : Parser) return Character is
    begin
        return Parser_Peek(Parzer, 0);
    end;

    function Parser_Is_EOF(Parzer : in Parser) return Boolean is
    begin
        return Parzer.Index >= Parzer.Length;
    end;

    procedure Parser_Advance(Parzer : in out Parser) is
    begin
        if not Parser_Is_EOF(Parzer) then
            Parzer.Index := Parzer.Index + 1;
        end if;
    end;

    procedure Parser_Skip_Whitespace(Parzer : in out Parser) is
    begin
        while not Parser_Is_EOF(Parzer) and Is_Whitespace(Parser_Token(Parzer)) loop
            Parser_Advance(Parzer);
        end loop;
    end;

    function Parser_Parse_String(Parzer : in out Parser) return Unbounded_String is
        Ch : Character;
        Is_Reading_Escaped_Char : Boolean := false;
        Res : Unbounded_String;
    begin
        Ch := Parser_Token(Parzer);
        Assert(Ch = '"');
        Parser_Advance(Parzer);

        while not Parser_Is_EOF(Parzer) loop
            Ch := Parser_Token(Parzer);

            if Is_Reading_Escaped_Char then
                Res := Res & Ch;
                Is_Reading_Escaped_Char := false;
            elsif Ch = '\' then
                Is_Reading_Escaped_Char := true;
            elsif Ch = '"' then
                exit;
            else
                Res := Res & Ch;
            end if;
            
            Parser_Advance(Parzer);
        end loop;

        Assert(Ch = '"');
        Parser_Advance(Parzer);

        return Res;
    end;

    function Parser_Parse_Next(Parzer : in out Parser) return JsonNode is
        Ch : Character;
        Result : JsonNode;
    begin
        Parser_Skip_Whitespace(Parzer);
        Ch := Parser_Token(Parzer);
        Result := (case Ch is
            when '"' => (Kind => JsonStr, Str => Parser_Parse_String(Parzer)),
            when others => (Kind => JsonNull));
        return Result;
    end;

    function Parse(S : String) return JsonNode is
    begin
        return Parse(To_Unbounded_String(S));
    end;

    function Parse(S : Unbounded_String) return JsonNode is
        Parzer : Parser := (S, Length(S), 0);
    begin
        return Parser_Parse_Next(Parzer);
    end;
end;
