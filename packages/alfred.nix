{ fetchurl, lib, stdenv, undmg, ... }:

with lib;
stdenv.mkDerivation {
  pname = "alfred";
  version = "5.0.6_2110";

  src = fetchurl {
    url = "https://cachefly.alfredapp.com/Alfred_5.0.6_2110.dmg";
    hash = "sha256-ziY41r8vFp6J9VFdUcks3S24qf1odGLeQfwLETvzS5o=";
  };

  nativeBuildInputs = [ undmg ];

  sourceRoot = "Alfred 5.app";

  installPhase = ''
    mkdir -p $out/Applications
    cp -r . "$out/Applications/Alfred 5.app"
  '';

  meta = {
    homepage = "https://www.alfredapp.com";
    license = licenses.unfree;
    platforms = platforms.darwin;
  };
}
