{ fetchurl, lib, stdenv, undmg, ... }:

stdenv.mkDerivation rec {
  pname = "rectangle";
  version = "0.64";

  src = fetchurl {
    url = "https://github.com/rxhanson/Rectangle/releases/download/v${version}/Rectangle${version}.dmg";
    hash = "sha256-z4xji072hGUFnxwCmME3FxW+dZuzNKoMvMQ17oRZocc=";
  };

  nativeBuildInputs = [ undmg ];

  sourceRoot = ".";

  installPhase = ''
    mkdir -p $out/Applications
    cp -r Rectangle.app $out/Applications
  '';

  dontFixup = true;

  meta = with lib; {
    homepage = "https://github.com/rxhanson/Rectangle";
    license = licenses.mit;
    platforms = platforms.darwin;
  };
}
