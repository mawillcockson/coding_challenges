let
  languages = ["Nix" "Haskell" "Python"];
in
  builtins.concatStringsSep "\n" (map (l: "I love ${l}") languages) + "\n" + builtins.toString 1
