{ fetchurl, lib, stdenv, ... }:

with lib;
stdenv.mkDerivation rec {
  pname = "corretto11";
  version = "11.0.8.10.1";

  src = fetchurl {
    url =
      "https://corretto.aws/downloads/resources/${version}/amazon-corretto-${version}-macosx-x64.tar.gz";
    sha256 = "063qz47hfmr0zfv4kg1vi7c31y2jnihxyxihaig5q1w9q5kdblpi";
  };

  dontBuild = true;

  dontFixup = true;

  installPhase = ''
    cp -r Contents/Home "$out"
  '';

  meta = {
    license = licenses.gpl2;
    platforms = platforms.darwin;
  };
}
