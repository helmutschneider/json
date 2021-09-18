
Program json;

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

Function TestReadString() : TestResult;
Begin
  n := p.Parse('"yee!"');
  If Not (n is StringNode) Then
    Begin
      TestReadString := Fail;
      halt();
    End;
  If Not ((n as StringNode).Str = 'yee!') Then
    Begin
      TestReadString := Fail;
      halt();
    End;
  TestReadString := Pass;
End;

Var t : Test;

Var tests : Array[0..63] Of Test;

Var res : TestResult;

Var out : Integer;

Begin
  p.Init();

  tests[0] := Test.Init('Read string', @TestReadString);

  For t In tests Do
    Begin
      If t = Nil Then
        Begin
          break;
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
