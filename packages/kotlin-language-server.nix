{ fetchurl, lib, stdenv, unzip, ... }:

with lib;
stdenv.mkDerivation rec {
  pname = "kotlin-language-server";
  version = "0.7.0";

  src = fetchurl {
    url =
      "https://github.com/fwcd/kotlin-language-server/releases/download/${version}/server.zip";
    sha256 = "1m9mw29sg3dknhvzs30izk2k3jls114cvfl0fzsjxlr7b7xhxs5p";
  };

  nativeBuildInputs = [ unzip ];

  unpackCmd = ''unzip -o "$curSrc"'';

  installPhase = ''
    mkdir -p $out
    cp -r bin lib "$out"
  '';

  meta = {
    description =
      "Intelligent Kotlin support for any editor/IDE using the Language Server Protocol";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
