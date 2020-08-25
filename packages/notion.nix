{ fetchurl, lib, stdenv, undmg, ... }:

with lib;
stdenv.mkDerivation {
  pname = "notion";
  version = "2.0.8";

  src = fetchurl {
    url = "https://desktop-release.notion-static.com/Notion-2.0.8.dmg";
    sha256 = "0jlckhwyc027cwxj1afjkx6j9sj6w72cgcns7c0xk85zlc482nx6";
  };

  nativeBuildInputs = [ undmg ];

  sourceRoot = "Notion.app";

  installPhase = ''
    mkdir -p "''${out}/Applications/Notion.app"
    cp -R . "''${out}/Applications/Notion.app"
  '';

  meta = with stdenv.lib; {
    homepage = "https://www.notion.so";
    license = licenses.unfree;
    platforms = platforms.darwin;
  };
}
