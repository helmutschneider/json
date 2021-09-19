
Function IsWhitespace(chr : Char) : Boolean;

Begin
  // #9 = \t (tab)
  // #13 = \r (carriage return)
  IsWhitespace := (chr = ' ') Or (chr = sLineBreak) Or (chr = ''#9'')
                  Or (chr = ''#13'');
End;

Function IsNumberLike(chr : Char) : Boolean;
Begin
  IsNumberLike := (chr = '0') Or (chr = '1') Or (chr = '2') Or (chr = '3') Or (
                  chr = '4') Or (chr = '5') Or (chr = '6') Or (chr = '7') Or (
                  chr = '8') Or (chr = '9') Or (chr = '.');
End;

Type 
  Parser = Object
    Index : Integer;
    Buffer : String;

    Constructor Init;

    Private 
      Procedure Expect(chr : Char);
      Procedure SkipWhitespace();
      Procedure MoveNext();

      Function Peek() : Char;
      Function ReadNode() : Node;
      Function ReadString() : StringNode;
      Function ReadNumber() : NumberNode;
      Function ReadArray() : ArrayNode;
      Function ReadObject() : ObjectNode;

    Public 
      Function Parse(s: String) : Node;
  End;

  Constructor Parser.Init();
Begin
  Index := 1;
  Buffer := '';
End;

Procedure Parser.Expect(chr : Char);

Var found : Char;
Begin
  found := Self.Peek();
  If found <> chr Then
    Begin
      WriteLn('Expected ' + chr + ', found ' + found);
      halt(1);
    End;
End;

Procedure Parser.SkipWhitespace();

Begin
  While IsWhitespace(Self.Peek()) Do
    Begin
      Self.MoveNext();
    End;
End;

Function Parser.Peek() : Char;
Begin
  Peek := Buffer[Self.Index];
End;

Procedure Parser.MoveNext();
Begin
  Self.Index := Self.Index + 1;
End;

Function Parser.ReadNode() : Node;

Var chr : Char;
Begin
  Self.SkipWhitespace();
  chr := Self.Peek();

  If IsNumberLike(chr) Then
    Begin
      ReadNode := Self.ReadNumber();
    End;

  Case chr Of 
    '"' : ReadNode := Self.ReadString();
    '[' : ReadNode := Self.ReadArray();
    '{' : ReadNode := Self.ReadObject();
    't' :
          Begin
            ReadNode := BooleanNode.Init(true);
            Self.Index := Self.Index + 4;
          End;
    'f':
         Begin
           ReadNode := BooleanNode.Init(false);
           Self.Index := Self.Index + 5;
         End;
    'n':
         Begin
           ReadNode := NullNode.Init();
           Self.Index := Self.Index + 4;
         End;
  End;
End;

Function Parser.ReadString() : StringNode;

Var chr : Char;

Var str : String;
Begin
  str := '';
  Self.Expect('"');
  Self.MoveNext();

  While Self.Peek() <> '"'  Do
    Begin
      chr := Self.Peek();
      If chr = '\' Then
        Begin
          // move past the escape character.
          Self.MoveNext();
          str := str + Self.Peek();
          Self.MoveNext();
        End
      Else
        Begin
          str := str + chr;
          Self.Index := Self.Index + 1
        End
    End;

  Self.MoveNext();

  ReadString := StringNode.Init(str);
End;

Function Parser.ReadNumber() : NumberNode;

Var str : String;

Var ch : Char;
Begin
  str := '';
  While IsNumberLike(Self.Peek()) Do
    Begin
      str := str + Self.Peek();
      Self.MoveNext();
    End;

  ReadNumber := NumberNode.Init(StrToFloat(str));
End;

Function Parser.ReadArray() : ArrayNode;

Var nodes : NodeList;

Var i : Integer;
Begin
  Self.Expect('[');
  Self.MoveNext();
  i := 0;

  While Self.Peek() <> ']' Do
    Begin
      nodes[i] := Self.ReadNode();
      i := i + 1;
      Self.SkipWhitespace();
      If Self.Peek() = ',' Then
        Begin
          Self.MoveNext();
        End;
    End;

  Self.MoveNext();

  ReadArray := ArrayNode.Init(nodes);
End;

Function Parser.ReadObject() : ObjectNode;

Var i : Integer;

Var key : StringNode;

Var value : Node;

Var entries : ObjectEntryList;
Begin
  Self.Expect('{');
  Self.MoveNext();

  i := 0;

  While Self.Peek() <> '}' Do
    Begin
      Self.SkipWhitespace();
      key := Self.ReadString();

      Self.SkipWhitespace();
      Self.Expect(':');
      Self.MoveNext();
      value := Self.ReadNode();

      entries[i] := ObjectEntry.Init(key.Value, value);

      i := i + 1;

      Self.SkipWhitespace();

      If Self.Peek() = ',' Then
        Begin
          Self.MoveNext();
        End;
    End;

  Self.MoveNext();

  ReadObject := ObjectNode.Init(entries);
End;

Function Parser.Parse(s: String) : Node;

Begin
  // the string type is 1-indexed.
  Self.Index := 1;
  Self.Buffer := s;
  Parse := self.ReadNode();
End;
