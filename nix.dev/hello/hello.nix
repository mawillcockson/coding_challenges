{
  stdenv,
  fetchzip,
}:
stdenv.mkDerivation {
  pname = "hello";
  version = "2.12.1";

  src = fetchzip {
    url = "https://ftpmirror.gnu.org/gnu/hello/hello-2.12.1.tar.gz";
    hash = "sha256-1kJjhtlsAkpNB7f6tZEs+dbKd8z7KoNHyDHEJ0tmhnc=";
  };
}
