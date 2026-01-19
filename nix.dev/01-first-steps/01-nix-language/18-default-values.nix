let
  f = {
    a,
    b ? 1,
  }:
    a + b;
in [
  (f {a = 1;})
  (f {
    a = 1;
    b = 2;
  })
]
