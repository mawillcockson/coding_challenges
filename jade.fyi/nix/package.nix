{
  # receives(*) pkgs.hello and pkgs.stdenv
  hello,
  stdenvNoCC,
  # can be overridden with `yourPackage.override { traditionalGreeting = true; }`
  traditionalGreeting ? false,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  # optional finalAttrs to refer to the set below; preferred over using `rec` attr sets
  pname = "hello-runner";
  version = "0.0.0";
  nativeBuildInputs = [
    hello
  ];
  builder = builtins.toFile "build-script.sh" ''
    command -V hello
    hello${
      if traditionalGreeting
      then " -t"
      else ""
    } > "''${out:?"\$out not set"}"
  '';
})
