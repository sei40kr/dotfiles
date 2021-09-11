{ fetchzip, jre, lib, makeWrapper, stdenv, unzip, ... }:

with lib;
stdenv.mkDerivation rec {
  pname = "kotlin-language-server";
  version = "1.1.2";

  src = fetchzip {
    url =
      "https://github.com/fwcd/kotlin-language-server/releases/download/${version}/server.zip";
    sha256 = "021h9239lr19r9r726hfjlfgwa8fl4m8vfryzsg8fbm0hsziklkz";
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
