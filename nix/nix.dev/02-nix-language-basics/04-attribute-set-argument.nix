let
  attr_set = { a ? 1 }: a + 1;
  elemAt = builtins.elemAt;
  list_set = a: (elemAt a 0) + (elemAt a 1);
in (attr_set { a = 2; }) - (list_set [ 2 1 ])
