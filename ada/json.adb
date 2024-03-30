with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;

package body Json is
    type Parser is record
        Data : Unbounded_String;
        Length : Natural;
        Index : Positive; -- 1-indexed
    end record;

    JsonParseError : exception;

    function Length(S : Unbounded_String) return Natural is
    begin
       return Ada.Strings.Unbounded.Count(S, ISO_646_Set);
    end;

    function Char_At(S : Unbounded_String; I : Positive) return Character is
    begin
        return Ada.Strings.Unbounded.Element(S, I);
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
        case Left.Kind is
            when JsonNull =>
                return true;
            when JsonStr =>
                return Left.Str = Right.Str;
            when JsonBool =>
                return Left.Bool = Right.Bool;
            when JsonArray =>
                if Left.Items.Length /= Right.Items.Length then
                    return false;
                end if;
                for K in Left.Items.First_Index .. Left.Items.Last_Index loop
                    declare
                        A : JsonNodeAccess;
                        B : JsonNodeAccess;
                    begin
                        A := Left.Items(K);
                        B := Right.Items(K);

                        if not Equal(A.all, B.all) then
                            return false;
                        end if;
                    end;
                end loop;
                return true;
        end case;
    end;

    function Parser_Peek(P : Parser; Offset : Natural) return Character is
    begin
        return Char_At(P.Data, P.Index + Offset);
    end;

    function Parser_Token(P : Parser) return Character is
    begin
        return Parser_Peek(P, 0);
    end;

    function Parser_Is_EOF(P : in Parser) return Boolean is
    begin
        return P.Index > P.Length;
    end;

    procedure Parser_Advance(P : in out Parser; C : Positive) is
    begin
        if not Parser_Is_EOF(P) then
            P.Index := P.Index + C;
        end if;
    end;

    procedure Parser_Advance(P : in out Parser) is
    begin
        Parser_Advance(P, 1);
    end;

    function Parser_Parser_Null(P : in out Parser) return JsonNode is
        Ch : Character;
    begin
        Ch := Parser_Token(P);
        Assert(Ch = 'n');
        Parser_Advance(P, 4);
        return (Kind => JsonNull);
    end;

    procedure Parser_Skip_Whitespace(P : in out Parser) is
    begin
        while not Parser_Is_EOF(P) and Is_Whitespace(Parser_Token(P)) loop
            Parser_Advance(P);
        end loop;
    end;

    function Parser_Parse_String(P : in out Parser) return JsonNode is
        Ch : Character;
        Is_Reading_Escaped_Char : Boolean := false;
        Res : Unbounded_String;
    begin
        Ch := Parser_Token(P);
        Assert(Ch = '"');
        Parser_Advance(P);

        while not Parser_Is_EOF(P) loop
            Ch := Parser_Token(P);

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
            
            Parser_Advance(P);
        end loop;

        Assert(Ch = '"');
        Parser_Advance(P);

        return (Kind => JsonStr, Str => Res);
    end;

    function Parser_Parse_Boolean(P : in out Parser) return JsonNode is
        Ch : Character;
    begin
        Ch := Parser_Token(P);
        Assert(Ch = 't' or Ch = 'f');

        if Ch = 't' then
            Parser_Advance(P, 4);
            return (Kind => JsonBool, Bool => true);
        elsif Ch = 'f' then
            Parser_Advance(P, 5);
            return (Kind => JsonBool, Bool => false);
        end if;

        Assert(false);
        return (Kind => JsonNull);
    end;

    function Parser_Parse_Next(P : in out Parser) return JsonNode; -- predeclared, yee!
    function Parser_Parse_Array(P : in out Parser) return JsonNode is
        Ch : Character;
        Node : JsonNodeAccess;
        Items : JsonVectors.Vector;
        Res : JsonNode;
    begin
        Ch := Parser_Token(P);
        Assert(Ch = '[');
        Parser_Advance(P);

        while not Parser_Is_EOF(P) loop
            Node := new JsonNode'(Parser_Parse_Next(P));
            Items.Append(Node);
            Parser_Skip_Whitespace(P);
            Ch := Parser_Token(P);
            if Ch = ',' then
                Parser_Advance(P);
            elsif Ch = ']' then
                exit;
            end if;
        end loop;

        Ch := Parser_Token(P);
        Assert(Ch = ']');
        Parser_Advance(P);
        Res := (Kind => JsonArray, Items => Items);
        return Res;
    end;

    function Parser_Parse_Error(P : Parser) return JsonNode is
    begin
        raise JsonParseError with "Parse error at" & P.Index'Image;
        return (Kind => JsonNull);
    end;

    function Parser_Parse_Next(P : in out Parser) return JsonNode is
        Ch : Character;
        Result : JsonNode;
    begin
        Parser_Skip_Whitespace(P);
        Ch := Parser_Token(P);
        Result := (case Ch is
            when '"' => Parser_Parse_String(P),
            when 't' | 'f' => Parser_Parse_Boolean(P),
            when 'n' => Parser_Parser_Null(P),
            when '[' => Parser_Parse_Array(P),
            when others => Parser_Parse_Error(P));
        return Result;
    end;

    function Parse(S : String) return JsonNode is
    begin
        return Parse(To_Unbounded_String(S));
    end;

    function Parse(S : Unbounded_String) return JsonNode is
        P : Parser := (S, Length(S), 1);
    begin
        return Parser_Parse_Next(P);
    end;
end;
