let
  f = {
    a,
    b,
    ...
  }:
    a + b;
in [
  (f {
    a = 1;
    b = 2;
  })
  (f {
    a = 1;
    b = 2;
    c = 3; # no longer an error
  })
]
