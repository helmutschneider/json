
Type 
  NodeKind = (String_, Number, Boolean_, Array_, Object_, Null);

  Node = Class
    Public 
      Kind : NodeKind;
  End;

  NodeList = Array[0..15] Of Node;
  ObjectEntry = Class
    Public 
      Key : String;
      Value : Node;
      Constructor Init(k : String; v : Node);
  End;

  ObjectEntryList = Array[0..15] Of ObjectEntry;

  StringNode = Class(Node)
    Public 
      Value : String;
      Constructor Init(s: String);
  End;

  NumberNode = Class(Node)
    Public 
      Value : Real;
      Constructor Init(v : Real);
  End;

  ArrayNode = Class(Node)
    Public 
      Value : NodeList;
      Constructor Init(n : NodeList);
  End;

  BooleanNode = Class(Node)
    Public 
      Value: Boolean;
      Constructor Init(v : Boolean);
  End;

  NullNode = Class(Node)
    Public 
      Constructor Init();
  End;

  ObjectNode = Class(Node)
    Public 
      Value : ObjectEntryList;
      Constructor Init(e : ObjectEntryList);
  End;

  Constructor ObjectEntry.Init(k : String; v : Node);
Begin
  Key := k;
  Value := v;
End;

Constructor StringNode.Init(s : String);
Begin
  Kind := String_;
  Value := s;
End;

Constructor NumberNode.Init(v : Real);
Begin
  Kind := Number;
  Value := v;
End;

Constructor ArrayNode.Init(n : NodeList);
Begin
  Kind := Array_;
  Value := n;
End;

Constructor BooleanNode.Init(v : Boolean);
Begin
  Kind := Boolean_;
  Value := v;
End;

Constructor NullNode.Init();
Begin
  Kind := Null;
End;

Constructor ObjectNode.Init(e : ObjectEntryList);
Begin
  Kind := Object_;
  Value := e;
End;
