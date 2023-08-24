{ lib, fetchurl, stdenv, undmg }:

stdenv.mkDerivation rec {
  pname = "spark-desktop";
  version = "3.7.2.55042";

  src = fetchurl {
    url = "https://downloads.sparkmailapp.com/Spark3/mac/dist/${version}/Spark.dmg";
    hash = "sha256-Plm66FRqj+fCjmE//6XoAM/iMOJa/nkG1URbRPOLUKs=";
  };

  nativeBuildInputs = [ undmg ];

  sourceRoot = "Spark Desktop.app";

  installPhase = ''
    runHook preInstall

    mkdir -p $out/Applications/Amphetamine\ Enhancer.app
    cp -R . $out/Applications/Spark\ Desktop.app

    runHook postInstall
  '';

  meta = with lib; {
    description = "Fast, cross-platform email designed to filter out the noise - so you can focus on what's important.";
    homepage = "https://sparkmailapp.com";
    license = licenses.unfree;
    platforms = platforms.darwin;
  };
}
