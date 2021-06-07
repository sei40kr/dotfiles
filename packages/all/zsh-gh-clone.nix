{ fetchFromGitHub, lib, stdenv, ... }:

with lib;
stdenv.mkDerivation {
  pname = "zsh-gh-clone";
  version = "unstable-2020-01-25";

  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "zsh-gh-clone";
    rev = "e5fa7b5aa62f1acc388ee3dcc0f4cf4352184a53";
    sha256 = "0xw8w3ss48lsq21dhdfyxral5qqc1yjv8yz8v3vyih7lk10cnpy2";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/zsh/site-functions
    cp functions/gh-clone $out/share/zsh/site-functions
  '';

  meta.platforms = platforms.all;
}
