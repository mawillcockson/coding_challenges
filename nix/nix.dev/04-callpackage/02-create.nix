{ writeShellScriptBin }:
writeShellScriptBin "configLocation" ''
  printf 'configurations are placed in: %s\n' ''${XDG_CONFIG_HOME}
''
