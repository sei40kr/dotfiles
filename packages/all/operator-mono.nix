{ lib, stdenv, ... }:

with lib;
stdenv.mkDerivation {
  pname = "operator-mono";
  version = "unstable-2020-07-26";

  src = builtins.fetchGit {
    url = "ssh://git@github.com/sei40kr/operator-mono.git";
    rev = "abda390811469158511b5633579c906d18c80a8b";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/fonts/opentype
    cp *.otf $out/share/fonts/opentype
  '';

  meta.platforms = platforms.all;
}
