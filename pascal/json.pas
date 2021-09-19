
Program json;

Uses SysUtils;

{$INCLUDE node.pas}
{$INCLUDE parser.pas}

Type 
  TestResult = (Pass, Fail);
  TestFn = Function () : TestResult;
  Test = Class
    Name : String;
    Fn : TestFn;

    Constructor Init(n : String; f: TestFn);
  End;

  Constructor Test.Init(n : String; f: TestFn);
Begin
  Name := n;
  Fn := f;
End;

Var p : Parser;

Var n : Node;

Function TestParseString() : TestResult;
Begin
  n := p.Parse('"yee!"');
  If Not (n is StringNode) Then
    Begin
      TestParseString := Fail;
      exit;
    End;
  If Not ((n as StringNode).Value = 'yee!') Then
    Begin
      TestParseString := Fail;
      exit;
    End;
  TestParseString := Pass;
End;

Function TestParseStringWithEscapedCharacters() : TestResult;
Begin
  n := p.Parse('"\"b"');
  If Not (n is StringNode) Then
    Begin
      TestParseStringWithEscapedCharacters := Fail;
      exit;
    End;
  If Not ((n as StringNode).Value = '"b') Then
    Begin
      TestParseStringWithEscapedCharacters := Fail;
      exit;
    End;
  TestParseStringWithEscapedCharacters := Pass;
End;

Function TestParseEmptyArray() : TestResult;
Begin
  n := p.Parse('[]');
  If Not (n Is ArrayNode) Then
    Begin
      TestParseEmptyArray := Fail;
      exit;
    End;
  TestParseEmptyArray := Pass;
End;

Function TestParseArrayOfStrings() : TestResult;
Begin
  n := p.Parse('[  "a",  "b"  ]');
  If Not (n Is ArrayNode) Then
    Begin
      TestParseArrayOfStrings := Fail;
      exit;
    End;
  If Not (((n as ArrayNode).Value[0] as StringNode).Value = 'a') Then
    Begin
      TestParseArrayOfStrings := Fail;
      exit;
    End;
  If Not (((n as ArrayNode).Value[1] as StringNode).Value = 'b') Then
    Begin
      TestParseArrayOfStrings := Fail;
      exit;
    End;
  TestParseArrayOfStrings := Pass;
End;

Function TestParseBooleans() : TestResult;
Begin
  n := p.Parse('true');
  If Not (n is BooleanNode) Then
    Begin
      TestParseBooleans := Fail;
      exit;
    End;
  If Not ((n as BooleanNode).Value = true) Then
    Begin
      TestParseBooleans := Fail;
      exit;
    End;
  n := p.Parse('false');
  If Not (n is BooleanNode) Then
    Begin
      TestParseBooleans := Fail;
      exit;
    End;
  If Not ((n as BooleanNode).Value = false) Then
    Begin
      TestParseBooleans := Fail;
      exit;
    End;
  TestParseBooleans := Pass;
End;

Function TestParseNull() : TestResult;
Begin
  n := p.Parse('null');
  If Not (n is NullNode) Then
    Begin
      TestParseNull := Fail;
      exit;
    End;
  TestParseNull := Pass;
End;

Function TestParseObject() : TestResult;

Var o : ObjectNode;
Begin
  o := p.Parse('{ "a" : "b", '#9' '#10' "c": [ "d" ] }') as ObjectNode;

  If Not (o.Value[0].Key = 'a') Then
    Begin
      TestParseObject := Fail;
      exit;
    End;

  If Not ((o.Value[0].Value as StringNode).Value = 'b') Then
    Begin
      TestParseObject := Fail;
      exit;
    End;

  If Not (o.Value[1].Key = 'c') Then
    Begin
      TestParseObject := Fail;
      exit;
    End;

  If Not (((o.Value[1].Value as ArrayNode).Value[0] as StringNode).Value = 'd')
    Then
    Begin
      TestParseObject := Fail;
      exit;
    End;

  TestParseObject := Pass;
End;

Function TestParseNumber() : TestResult;

Var num : NumberNode;
Begin
  num := p.Parse('123.5') as NumberNode;
  If num.Value <> 123.5 Then
    Begin
      TestParseNumber := Fail;
      exit;
    End;
  TestParseNumber := Pass;
End;

Var t : Test;

Var tests : Array[0..15] Of Test;

Var res : TestResult;

Var out : Integer;

Begin
  p.Init();

  tests[0] := Test.Init('Parse string', @TestParseString);
  tests[1] := Test.Init(
              'Parse string with escaped characters', @
              TestParseStringWithEscapedCharacters
              );
  tests[2] := Test.Init('Parse empty array', @TestParseEmptyArray);
  tests[3] := Test.Init('Parse array of strings', @TestParseArrayOfStrings);
  tests[4] := Test.Init('Parse booleans', @TestParseBooleans);
  tests[5] := Test.Init('Parse null', @TestParseNull);
  tests[6] := Test.Init('Parse object', @TestParseObject);
  tests[7] := Test.Init('Parse number', @TestParseNumber);

  For t In tests Do
    Begin
      If t = Nil Then
        Begin
          continue;
        End;
      res := t.Fn();
      If res = Pass Then
        Begin
          WriteLn('[OK] ' + t.Name);
        End
      Else
        Begin
          WriteLn('[FAIL] ' + t.Name);
          out := 1;
        End;
    End;

  halt(out);
End.
