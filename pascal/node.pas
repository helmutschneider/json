
Type 
  NodeKind = (String_, Number, Boolean_, Array_, Object_, Null);

  Node = Class
    Public 
      Kind : NodeKind;
  End;

  NodeList = Array[1..16] Of Node;

  StringNode = Class(Node)
    Public 
      Str : String;
      Constructor Init(s: String);
  End;

  ArrayNode = Class(Node)
    Public 
      Nodes : NodeList;
      Constructor Init(n : NodeList);
  End;

  Constructor StringNode.Init(s: String);
Begin
  Kind := String_;
  Str := s;
End;

Constructor ArrayNode.Init(n : NodeList);
Begin
  Kind := Array_;
  Nodes := n;
End;
