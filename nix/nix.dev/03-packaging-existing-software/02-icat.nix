{
  stdenv,
  fetchFromGitHub,
  imlib2,
  xorg,
  breakpointHook,
  less,
}:
stdenv.mkDerivation {
  pname = "icat";
  version = "v0.5";
  src = fetchFromGitHub {
    owner = "atextor";
    repo = "icat";
    rev = "v0.5";
    hash = "sha256-aiLPVdKSppT/PWPW0Ue475WG61pBLh8OtLuk2/UU3nM=";
  };

  #nativeBuildInputs = [breakpointHook];
  buildInputs = [imlib2 xorg.libX11 less];
  installPhase = [
    {
      __toString = {...}:
        break ''
          runHook preInstall
          mkdir -p "$out/bin"
          cp -v icat "$out/bin"
          runHook postInstall
        '';
    }
  ];
}
