{ writeShellScriptBin, audience ? "World" }:
writeShellScriptBin "hello" ''
  echo 'Hello, ${audience}!'
''
