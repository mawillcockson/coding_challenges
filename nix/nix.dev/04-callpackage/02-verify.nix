{ writeShellScriptBin, create, run_with_env, uutils-coreutils-noprefix }:
writeShellScriptBin "verify" ''
  if ! OUTPUT="$(${run_with_env}/bin/run_with_env "${create}/bin/configLocation")"; then
    echo 'problem with run_with_env'
    exit 1
  fi
  if ! USER="''${USER:-"$("${uutils-coreutils-noprefix}/bin/id" -un)"}"; then
    echo 'problem fetching $USER'
    exit 1
  fi
  OUTPUT="''${OUTPUT#"configurations are placed in: "}"
  if test "$USER" = "root"; then
    if test "$OUTPUT" != "/root/.config"; then
      printf 'expected root to have config in /root/.config, not: %s\n' "$OUTPUT"
      exit 1
    fi
  elif test "$OUTPUT" != "/home/$USER/.config"; then
    printf 'expected /home/$USER/.config, not: %s\n' "$OUTPUT"
    exit 1
  fi
  echo 'everything worked and is as expected'
  exit 0
''
