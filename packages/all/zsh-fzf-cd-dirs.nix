{ fetchFromGitHub, lib, stdenv, ... }:

with lib;
stdenv.mkDerivation {
  pname = "zsh-fzf-cd-dirs";
  version = "unstable-2020-01-09";

  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "zsh-fzf-cd-dirs";
    rev = "c59dde5c63821d98bb2fab5dd7a3a308df8a3594";
    sha256 = "05nff854spaqcqwn0yhr6pkpqh7kd02nvh7nhr8fbi6nb2ywh98l";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/zsh/site-functions
    cp functions/* $out/share/zsh/site-functions
  '';

  meta.platforms = platforms.all;
}
