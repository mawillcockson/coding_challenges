{ stdenv, fetchzip,
/* pkgs ? (import (fetchTarball (import ../../default-nixpkgs-url.nix))) {
     config = { };
     overlays = [ ];
   }
*/
}:
stdenv.mkDerivation rec {
  pname = "hello";
  version = "2.12.1";

  src = fetchzip {
    url = "https://ftpmirror.gnu.org/gnu/${pname}/${pname}-${version}.tar.gz";
    hash = "sha256-1kJjhtlsAkpNB7f6tZEs+dbKd8z7KoNHyDHEJ0tmhnc=";
  };
}
