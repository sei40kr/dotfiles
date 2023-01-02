{ fetchurl, lib, stdenv, undmg, ... }:

stdenv.mkDerivation rec {
  pname = "rectangle";
  version = "0.64";

  src = fetchurl {
    url = "https://github.com/rxhanson/Rectangle/releases/download/v${version}/Rectangle${version}.dmg";
    hash = "sha256-z4xji072hGUFnxwCmME3FxW+dZuzNKoMvMQ17oRZocc=";
  };

  nativeBuildInputs = [ undmg ];

  sourceRoot = "Rectangle.app";

  installPhase = ''
    mkdir -p $out/Applications/Rectangle.app
    cp -R . $out/Applications/Rectangle.app
  '';

  meta = with lib; {
    homepage = "https://github.com/rxhanson/Rectangle";
    license = licenses.mit;
    platforms = platforms.darwin;
  };
}
