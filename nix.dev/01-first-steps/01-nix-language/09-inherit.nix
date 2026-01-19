let
  x = 1;
  y = 2;
  z = {
    a.b = "a.b";
    c = 3;
  };
  zz = {a = 1;};
  zzz = {
    b = 2;
    c = 3;
  };
in {
  inherit x y z;
  inherit (zz) a;
  zzz = {
    inherit (zzz) b;
    inherit zz;
  };
  # doesn't work
  # zzz2 = {inherit zz.a;};
}
