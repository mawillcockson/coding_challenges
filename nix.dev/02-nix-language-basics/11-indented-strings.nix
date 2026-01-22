let
  t = ''
    a very long string
    that can be split over
    multiple lines, without
    the indentation the left
    being counted as part
    of the string
  '';
  a = ''
    this only works if the file
    is formatted with unix-style,
    LF-only line-endings
  '';
in
  builtins.concatStringsSep "\n" [t a]
