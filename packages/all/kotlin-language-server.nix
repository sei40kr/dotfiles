{ fetchzip, jre, lib, makeWrapper, stdenv, unzip, ... }:

with lib;
stdenv.mkDerivation rec {
  pname = "kotlin-language-server";
  version = "1.2.0";

  src = fetchzip {
    url =
      "https://github.com/fwcd/kotlin-language-server/releases/download/${version}/server.zip";
    sha256 = "1wxh3k3m2mia1p5g9njcyjrzx021dvz571056mwzsd84prh491d2";
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ jre ];

  installPhase = ''
    install -Dm755 bin/kotlin-language-server -t $out/bin
    cp -r lib $out/lib

    wrapProgram $out/bin/kotlin-language-server --prefix PATH : ${jre}/bin
  '';

  meta = {
    description =
      "Intelligent Kotlin support for any editor/IDE using the Language Server Protocol";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
