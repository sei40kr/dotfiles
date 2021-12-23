{ fetchurl, lib, stdenv, unzip, ... }:

with lib;
stdenv.mkDerivation rec {
  pname = "dash";
  version = "v6";

  src = fetchurl {
    url = "https://tokyo.kapeli.com/downloads/${version}/Dash.zip";
    sha256 = "05xj8rfprh32gj49c7zn6kdh66ndv7i9sb16aws689aay1qmi0cg";
  };

  buildInputs = [ unzip ];

  sourceRoot = "Dash.app";

  installPhase = ''
    mkdir -p "''${out}/Applications/Dash.app"
    cp -R . "''${out}/Applications/Dash.app"
  '';

  meta = {
    homepage = "https://kapeli.com/dash";
    license = licenses.unfree;
    platforms = platforms.darwin;
  };
}
