{ fetchzip, jre, lib, makeWrapper, stdenv, unzip, ... }:

with lib;
stdenv.mkDerivation rec {
  pname = "kotlin-language-server";
  version = "1.1.1";

  src = fetchzip {
    url =
      "https://github.com/fwcd/kotlin-language-server/releases/download/${version}/server.zip";
    sha256 = "12gnxhmdkcr2wp789ia8l9xhh26v7h8ckwlhvm8m2xzxq8nvavv8";
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ jre ];

  installPhase = ''
    install -D bin/kotlin-language-server -t "''${out}/bin"
    cp -r lib "''${out}/lib"

    wrapProgram "''${out}/bin/kotlin-language-server" \
        --prefix PATH : ${jre}/bin
  '';

  meta = {
    description =
      "Intelligent Kotlin support for any editor/IDE using the Language Server Protocol";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
