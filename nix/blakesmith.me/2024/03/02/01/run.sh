#!/usr/bin/env nix-shell
#! nix-shell -i dash --pure
#! nix-shell -p dash nix netcat 
##! nix-shell -p uutils-coreutils
#! nix-shell -I nixpkgs=channel:nixos-25.11-small
set -eu
NIX_CONFIG='extra-experimental-features = nix-command flakes pipe-operators
allow-import-from-derivation = false'
export NIX_CONFIG

log() {
    printf '%s %s\n' "--INFO--" "$1"
}
cleanup() {
    if test -n "${PID:+"set"}"; then
        log "killing ${PID}"
        kill "${PID}" || echo "could not kill ${PID}"
    fi
    trap - EXIT QUIT TERM
}
trap cleanup EXIT QUIT TERM

log 'building helloNixosTests...'
nix build .#helloNixosTests

log 'running helloNixosTests...'
./result/bin/hello-nixos-tests &
PID="$!"

log 'waiting for netcat to be ready...'
#TIMEOUT="${TIMEOUT:-"10"}"
#ENDTIME="$(date --date="now +${TIMEOUT} seconds" +%s)"
sleep 1
#while {
#    test "$(date +%s)" -lt "${ENDTIME}" &&
#        ! nc -dz localhost 3000
#    }
#do
#    sleep 1
#done
#if test "$(date +%s)" -ge "${ENDTIME}"; then
#    printf '%s %s\n' --ERROR-- "timeout elapsed"
#    exit 1
#fi
log 'netcat is hopefully ready'

log 'sending "hello"s to localhost:3000'
printf '%s\n' 'hello' 'this is a test' 'how are you?' |
    nc -N localhost 3000
log 'netcat should have exited, but it may not have'
wait
unset -v PID
