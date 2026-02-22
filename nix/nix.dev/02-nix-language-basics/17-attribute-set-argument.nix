let
  f = {
    a,
    b,
  }:
    a + b;
in
  f {
    a = 1;
    b = 2;
 /* c = 3; */ # this would be an error
  }
