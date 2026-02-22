{
  hello-wrapper,
  stdenvNoCC,
}:
stdenvNoCC.mkDerviation {
  pname = "hello-runner";
  version = "0.0.0";
  builder = ''
    ${hello-wrapper} > "''${out:?"\$out not set"}"
  '';
}
