{ fetchFromGitHub, lib, stdenv, ... }:

with lib;
stdenv.mkDerivation {
  pname = "zsh-fzf-chdir";
  version = "unstable-2021-06-08";

  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "zsh-fzf-chdir";
    rev = "82fa9f0a101b08fcc9bbc090ca259cb69ee047db";
    sha256 = "0absia8shn7dy5jz7922cd6spv35kblq7rq40d71z8vmhnyhj195";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/zsh/site-functions
    cp functions/* $out/share/zsh/site-functions
  '';

  meta.platforms = platforms.all;
}
