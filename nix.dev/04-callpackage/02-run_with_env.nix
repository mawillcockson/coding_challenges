{ writeShellScriptBin }:
writeShellScriptBin "run_with_env" ''
  XDG_CONFIG_HOME="''${HOME:-~}/.config"
  export XDG_CONFIG_HOME
  . "$@"
''
