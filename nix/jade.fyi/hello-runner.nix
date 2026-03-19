{
  hello-wrapper,
  stdenvNoCC,
  coreutils,
}:
stdenvNoCC.mkDerivation {
  pname = "hello-runner";
  version = "0.0.0";
  nativeBuildInputs = [
    hello-wrapper
    coreutils
  ];
  builder = builtins.toFile "builder.sh" ''
    hello-wrapper | tee "''${out:?"\$out not set"}"
  '';
}
