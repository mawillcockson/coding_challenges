{
  hello-wrapper,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation {
  pname = "hello-runner";
  version = "0.0.0";
  nativeBuildInputs = [hello-wrapper];
  builder = builtins.toFile "builder.sh" ''
    hello-wrapper > "''${out:?"\$out not set"}"
  '';
}
