let
  pkgs = (import ../../default-nixpkgs.nix).pkgs;
  inherit
    (pkgs.lib)
    lists
    strings
    filesystem
    path
    ;
  inherit (pkgs) lib;
  inherit
    (builtins)
    filter
    split
    isList
    head
    ;
in
  lib.evalModules {
    specialArgs = {inherit pkgs;};
    modules = [
      (path.append ./. (
        filesystem.listFilesRecursive ./.
        |> map baseNameOf
        |> filter (strings.hasSuffix "default.nix")
        |> lists.sortOn (
          f: split "([[:digit:]]+)" f |> filter (l: (isList l) && (l != [])) |> head |> head
        )
        |> lists.reverseList
        |> head
      ))
    ];
  }
