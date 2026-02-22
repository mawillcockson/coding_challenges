let
  f = args @ {
    a,
    b,
  }:
    args.a + args.b;
  g = {
    a,
    b,
    ...
  } @ args:
    a
    + b
    + (
      if args ? c
      then args.c
      else 3
    );
in [
  (f {
    a = 1;
    b = 2;
  })
  (g {
    a = 1;
    b = 2;
  })
  (g {
    a = 1;
    b = 2;
    c = 4;
  })
]
