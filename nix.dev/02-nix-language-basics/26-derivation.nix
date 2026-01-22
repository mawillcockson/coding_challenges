let
  nixpkgs = fetchTarball (import ../../default-nixpkgs-url.nix);
  pkgs = import nixpkgs {
    config = { };
    overrides = [ ];
  };
  derivation_ = derivation {
    name = "example";
    system = builtins.currentSystem;
    builder = /bin/sh;
    args = [ "-c" ''printf '%s\n' "$MESSAGE" > "$out"'' ];
    outputs = [ "out" ];
    MESSAGE = "Hello, Derivations!";
    allowedReferences = [ ];
    allowedRequisites = [ ];
  };
in builtins.trace (builtins.deepSeq derivation_ derivation_) "${derivation_}"
