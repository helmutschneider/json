with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers;             use Ada.Containers;

package body Json is
    type Parser is record
        Data   : Unbounded_String;
        Length : Natural;
        Index  : Positive; -- 1-indexed
    end record;

    type Position is record
        Line: Positive;
        Column: Positive;
    end record;

    JsonParseError : exception;

    function Is_Whitespace (Ch : Character) return Boolean is
    begin
        return Ch = ' '
            or Ch = ASCII.LF  -- \n
            or Ch = ASCII.CR  -- \r
            or Ch = ASCII.HT; -- \t
    end Is_Whitespace;

    function Char_At (S : Unbounded_String; Index : Positive) return Character is
    begin
        return Ada.Strings.Unbounded.Element(S, Index);
    end;

    function To_String (Node : JsonNode; Depth : Natural) return String is
        Indent : Unbounded_String;
        S      : Unbounded_String;
        Tab    : constant String := "  ";
        use Json.JsonMaps;
    begin
        for K in 1 .. Depth loop
            Indent := Indent & Tab;
        end loop;
        S      := S & Indent & Node.Kind'Image & ":" & ASCII.LF;
        Indent := Indent & Tab;
        case Node.Kind is
            when JsonNull =>
                S := S & Indent & "null";
            when JsonBoolean =>
                S := S & Indent & Node.Bool'Image;
            when JsonNumber =>
                S := S & Indent & Node.Num'Image;
            when JsonString =>
                S := S & Indent & """" & Node.Str & """";
            when JsonArray =>
                for Child of Node.Vec loop
                    S := S & To_String (Child.all, Depth + 1) & ASCII.LF;
                end loop;
            when JsonObject =>
                for C in Node.Map.Iterate loop
                    S := S & Indent & """" & Key (C) & """:" & ASCII.LF;
                    S := S & To_String (Node.Map (Key (C)).all, Depth + 2) & ASCII.LF;
                end loop;
            when others =>
                null;
        end case;
        return To_String (S);
    end To_String;

    function To_String (Node : JsonNode) return String is
    begin
        return To_String (Node, 0);
    end To_String;

    function Equal (Left : JsonNode; Right : JsonNode) return Boolean is
        use Json.JsonMaps;
    begin
        if Left.Kind /= Right.Kind then
            return False;
        end if;
        case Left.Kind is
            when JsonNull =>
                return True;
            when JsonBoolean =>
                return Left.Bool = Right.Bool;
            when JsonNumber =>
                return Left.Num = Right.Num;
            when JsonString =>
                return Left.Str = Right.Str;
            when JsonArray =>
                if Left.Vec.Length /= Right.Vec.Length then
                    return False;
                end if;
                for K in Left.Vec.First_Index .. Left.Vec.Last_Index loop
                    declare
                        A : JsonNodeAccess;
                        B : JsonNodeAccess;
                    begin
                        A := Left.Vec (K);
                        B := Right.Vec (K);

                        if not Equal (A.all, B.all) then
                            return False;
                        end if;
                    end;
                end loop;
                return True;
            when JsonObject =>
                if Left.Map.Length /= Right.Map.Length then
                    return False;
                end if;
                for C in Left.Map.Iterate loop
                    declare
                        A : JsonNodeAccess;
                        B : JsonNodeAccess;
                    begin
                        A := Left.Map (Key (C));
                        if not Right.Map.Contains (Key (C)) then
                            return False;
                        end if;
                        B := Right.Map (Key (C));
                        if not Equal (A.all, B.all) then
                            return False;
                        end if;
                    end;
                end loop;
                return True;
        end case;
    end Equal;

    function Parser_Peek (P : Parser; Offset : Natural) return Character is
    begin
        return Char_At (P.Data, P.Index + Offset);
    end Parser_Peek;

    function Parser_Peek (P : Parser) return Character is
    begin
        return Parser_Peek(P, 0);
    end Parser_Peek;

    function Parser_Position (P : Parser) return Position is
        Line : Positive := 1;
        Column : Natural := 1;
        Index : Positive := 1;
        Ch : Character;
    begin
        while Index /= P.Index loop
            Ch := Char_At(P.Data, Index);
            if Ch = ASCII.LF then
                Line := Line + 1;
                Column := 0;
            end if;
            if Ch /= ASCII.CR then
                Column := Column + 1;
            end if;
            Index := Index + 1;
        end loop;
        return (Line, Column);
    end;

    function Parser_Is_EOF (P : in Parser; Offset : Natural) return Boolean is
    begin
        return (P.Index + Offset) > P.Length;
    end Parser_Is_EOF;

    function Parser_Is_EOF (P : in Parser) return Boolean is
    begin
        return Parser_Is_EOF (P, 0);
    end Parser_Is_EOF;

    function Parser_Read_Erroneous_Word (P : Parser) return String is
        Offset : Natural := 0;
        Ch : Character;
        Word : Unbounded_String;
        Is_Delimiter : Boolean;
    begin    
        while not Parser_Is_EOF(P, Offset) loop
            Ch := Parser_Peek(P, Offset);
            Is_Delimiter := Is_Whitespace(Ch) or (case Ch is
                when ',' | '"' | '[' | ']' | '{' | '}' => true,
                when others => false
            );
            if Is_Delimiter then
                exit;
            end if;
            Word := Word & Ch;
            Offset := Offset + 1;
        end loop;
        return To_String(Word);
    end;

    function Parser_Read_Line (P : Parser) return String is
        StartIndex : Positive := P.Index;
        EndIndex : Positive := P.Index;
        Ch : Character;
    begin
        while StartIndex > 1 loop
            Ch := Char_At(P.Data, StartIndex - 1);
            if Ch = ASCII.LF or Ch = ASCII.CR then
                exit;
            end if;
            StartIndex := StartIndex - 1;
        end loop;
        while EndIndex < P.Length loop
            Ch := Char_At(P.Data, EndIndex + 1);
            if Ch = ASCII.LF or Ch = ASCII.CR then
                exit;
            end if;
            EndIndex := EndIndex + 1;
        end loop;
        return To_String(P.Data)(StartIndex .. EndIndex);
    end;

    procedure Parser_Raise_Error (P : Parser; Message : String) is
        Word : constant String := Parser_Read_Erroneous_Word(P);
        Pos : constant Position := Parser_Position(P);
        Line : constant String := Parser_Read_Line(P);
        Squiggly : Unbounded_String;
    begin
        for K in 1 .. (Pos.Column - 1) loop
            Squiggly := Squiggly & " ";
        end loop;
        for K in 1 .. Word'Length loop
            Squiggly := Squiggly & "^";
        end loop;

        raise JsonParseError with "Parse error at line" & Pos.Line'Image & ", column" & Pos.Column'Image & ": " & Message
            & ASCII.LF & "  " & Line & ASCII.LF & "  " & To_String(Squiggly);
    end Parser_Raise_Error;

    procedure Parser_Expect (P : Parser; Expected : String) is
        S : Unbounded_String;
    begin
        for K in 1 .. Expected'Length loop
            if Parser_Is_EOF (P, K - 1) then
                exit;
            end if;
            S := S & Parser_Peek (P, K - 1);
        end loop;
        if To_String (S) /= Expected then
            Parser_Raise_Error
               (P,
                "expected '" & Expected & "', found '" & To_String (S) & "'");
        end if;
    end Parser_Expect;

    procedure Parser_Advance (P : in out Parser; Offset : Positive) is
    begin
        if not Parser_Is_EOF (P) then
            P.Index := P.Index + Offset;
        end if;
    end Parser_Advance;

    procedure Parser_Advance (P : in out Parser) is
    begin
        Parser_Advance (P, 1);
    end Parser_Advance;

    function Parser_Parser_Null (P : in out Parser) return JsonNode is
    begin
        Parser_Expect (P, "null");
        Parser_Advance (P, 4);
        return (Kind => JsonNull);
    end Parser_Parser_Null;

    procedure Parser_Skip_Whitespace (P : in out Parser) is
    begin
        while not Parser_Is_EOF (P) and Is_Whitespace (Parser_Peek (P)) loop
            Parser_Advance (P);
        end loop;
    end Parser_Skip_Whitespace;

    function Parser_Parse_String (P : in out Parser) return JsonNode is
        Ch                      : Character;
        Is_Reading_Escaped_Char : Boolean := False;
        Res                     : Unbounded_String;
    begin
        Parser_Expect (P, """");
        Parser_Advance (P);

        while not Parser_Is_EOF (P) loop
            Ch := Parser_Peek (P);

            if Is_Reading_Escaped_Char then
                Res                     := Res & Ch;
                Is_Reading_Escaped_Char := False;
            elsif Ch = '\' then
                Is_Reading_Escaped_Char := True;
            elsif Ch = '"' then
                exit;
            else
                Res := Res & Ch;
            end if;

            Parser_Advance (P);
        end loop;

        Parser_Expect (P, """");
        Parser_Advance (P);

        return (Kind => JsonString, Str => Res);
    end Parser_Parse_String;

    function Parser_Parse_Boolean (P : in out Parser) return JsonNode is
        Ch : Character;
    begin
        Ch := Parser_Peek (P);

        if Ch = 't' then
            Parser_Expect (P, "true");
            Parser_Advance (P, 4);
            return (Kind => JsonBoolean, Bool => True);
        elsif Ch = 'f' then
            Parser_Expect (P, "false");
            Parser_Advance (P, 5);
            return (Kind => JsonBoolean, Bool => False);
        end if;

        Parser_Raise_Error (P, "expected 'true' or 'false'");
        return (Kind => JsonNull);
    end Parser_Parse_Boolean;

    function Parser_Parse_Number (P : in out Parser) return JsonNode is
        Ch : Character;
        S  : Unbounded_String;
        F  : Long_Float;
    begin
        -- super naive float parser implementation; we just
        -- look for numbers and the dot character.
        while not Parser_Is_EOF (P) loop
            Ch := Parser_Peek (P);
            if Ch in '0' .. '9' or Ch = '.' then
                S := S & Ch;
            else
                exit;
            end if;
            Parser_Advance (P);
        end loop;
        F := Long_Float'Value (To_String (S));
        return (Kind => JsonNumber, Num => F);
    end Parser_Parse_Number;

    function Parser_Parse_Next
       (P : in out Parser) return JsonNode; -- predeclared, yee!
    function Parser_Parse_Array (P : in out Parser) return JsonNode is
        Ch   : Character;
        Node : JsonNodeAccess;
        Vec  : JsonVectors.Vector;
    begin
        Parser_Expect (P, "[");
        Parser_Advance (P);
        Parser_Skip_Whitespace (P);

        while not Parser_Is_EOF (P) and Parser_Peek (P) /= ']' loop
            Node := new JsonNode'(Parser_Parse_Next (P));
            Vec.Append (Node);
            Parser_Skip_Whitespace (P);
            Ch := Parser_Peek (P);
            if Ch = ',' then
                Parser_Advance (P);
            elsif Ch = ']' then
                null;
            else
                Parser_Raise_Error (P, "expected ',' or ']'");
            end if;
        end loop;

        Parser_Expect (P, "]");
        Parser_Advance (P);
        return (Kind => JsonArray, Vec => Vec);
    end Parser_Parse_Array;

    function Parser_Parse_Object (P : in out Parser) return JsonNode is
        Ch    : Character;
        Key   : Unbounded_String;
        Value : JsonNodeAccess;
        Map   : JsonMaps.Map;
    begin
        Parser_Expect (P, "{");
        Parser_Advance (P);
        Parser_Skip_Whitespace (P);

        while not Parser_Is_EOF (P) and Parser_Peek (P) /= '}' loop
            Parser_Skip_Whitespace (P);
            Key := Parser_Parse_String (P).Str;
            Parser_Skip_Whitespace (P);
            Parser_Expect (P, ":");
            Parser_Advance (P);
            Value := new JsonNode'(Parser_Parse_Next (P));
            Map.Include (To_String (Key), Value);
            Parser_Skip_Whitespace (P);
            Ch := Parser_Peek (P);
            if Ch = ',' then
                Parser_Advance (P);
            elsif Ch = '}' then
                null;
            else
                Parser_Raise_Error (P, "expected ',' or '}'");
            end if;
        end loop;

        Parser_Expect (P, "}");
        Parser_Advance (P);

        return (Kind => JsonObject, Map => Map);
    end Parser_Parse_Object;

    function Parser_Parse_Next (P : in out Parser) return JsonNode is
        Ch     : Character;
        Result : JsonNode;
    begin
        Parser_Skip_Whitespace (P);
        Ch := Parser_Peek (P);
        case Ch is
            when 'n' =>
                return Parser_Parser_Null (P);
            when 't' | 'f' =>
                return Parser_Parse_Boolean (P);
            when '0' .. '9' =>
                return Parser_Parse_Number (P);
            when '"' =>
                return Parser_Parse_String (P);
            when '[' =>
                return Parser_Parse_Array (P);
            when '{' =>
                return Parser_Parse_Object (P);
            when others =>
                null;
        end case;

        Parser_Raise_Error (P, "expected a JSON node");
        return (Kind => JsonNull);
    end Parser_Parse_Next;

    function Parse (Str : String) return JsonNode is
    begin
        return Parse (To_Unbounded_String (Str));
    end Parse;

    function Parse (Str : Unbounded_String) return JsonNode is
        P : Parser;
    begin
        P := (Str, Length (Str), 1);
        return Parser_Parse_Next (P);
    end Parse;
end Json;
