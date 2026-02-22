rec {
  one = 1;
  two = one + 1;
  three = one + two;
  fib = x:
    if x == 1 then one else if x == 0 then 0 else (fib (x - 1)) + (fib (x - 2));
  fibs = map (fib) (builtins.genList (i: i) 10);
}
