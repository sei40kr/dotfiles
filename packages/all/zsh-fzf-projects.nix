{ fetchFromGitHub, lib, stdenv, ... }:

with lib;
stdenv.mkDerivation {
  pname = "zsh-fzf-projects";
  version = "unstable-2020-01-24";

  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "zsh-fzf-projects";
    rev = "ce061b33bd3c6c6392e4d4f3aaf90f593d4b404d";
    sha256 = "0wx9acrxj036w57h75w8xs68qij0bv9ixkcvxq1k8a3pyavhamcd";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/zsh/site-functions
    cp functions/fzf-projects $out/share/zsh/site-functions
  '';

  meta.platforms = platforms.all;
}
