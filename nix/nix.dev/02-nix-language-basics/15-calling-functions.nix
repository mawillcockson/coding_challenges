builtins.trace (let
  a = [
    (let f = x: x + 1; in f 2)
    (let f = x: x.a; in f {a = "attr";})
    ((x: x + 1) 2)
    (let f = x: x + 1; in [(f 2)])
    (let f = x: x + 1; in [f 2])
  ];
in
  builtins.deepSeq a a) "end"
