
Function IsWhitespace(chr : Char) : Boolean;

Begin
  IsWhitespace := (chr = ' ') Or (chr = '\n') Or (chr = '\t') Or (chr = '\r');
End;

Type 
  Parser = Object
    Index : Integer;
    Buffer : String;

    Constructor Init;

    Private 
      Procedure Expect(chr : Char);
      Procedure SkipWhitespace();

      Function Peek() : Char;
      Function ReadNode() : Node;
      Function ReadString() : StringNode;

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
      Self.Index := Self.Index + 1;
    End;
End;

Function Parser.Peek() : Char;
Begin
  Peek := Buffer[Self.Index];
End;

Function Parser.ReadNode() : Node;

Var n : Node;
Begin
  Self.SkipWhitespace();

  n := StringNode.Init('') as Node;
  ReadNode := n;
End;

Function Parser.ReadString() : StringNode;

Var chr : Char;

Var str : String;
Begin
  str := '';
  Self.Expect('"');
  Self.Index := Self.Index + 1;

  While Self.Peek() <> '"'  Do
    Begin
      chr := Self.Peek();

      If chr = '\\' Then
        Begin
          Self.Index := Self.Index + 1;
          str := str + Self.Peek();
        End
      Else
        Begin
          str := str + chr;
          Self.Index := Self.Index + 1
        End
    End;

  ReadString := StringNode.Init(str);
End;

Function Parser.Parse(s: String) : Node;

Begin
  Self.Index := 1;
  Self.Buffer := s;
  Parse := self.ReadString();
End;
