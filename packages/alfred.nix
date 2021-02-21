{ fetchurl, lib, stdenv, undmg, ... }:

with lib;
stdenv.mkDerivation {
  pname = "alfred";
  version = "4.1_1167";

  src = fetchurl {
    url = "https://cachefly.alfredapp.com/Alfred_4.1_1167.dmg";
    sha256 = "0gvwsb5b5kzvwkn31cby7kdg7fyf7sd99yibhhwdzsi4frma0kd8";
  };

  nativeBuildInputs = [ undmg ];

  sourceRoot = "Alfred 4.app";

  installPhase = ''
    mkdir -p "''${out}/Applications/Alfred 4.app"
    cp -R . "''${out}/Applications/Alfred 4.app"
  '';

  meta = {
    homepage = "https://www.alfredapp.com";
    license = licenses.unfree;
    platforms = platforms.darwin;
  };
}
