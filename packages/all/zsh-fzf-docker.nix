{ fetchFromGitHub, lib, stdenv, ... }:

with lib;
stdenv.mkDerivation {
  pname = "zsh-fzf-docker";
  version = "unstable-2020-01-24";

  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "zsh-fzf-docker";
    rev = "0bc3f104d7359db1306104f1fc8533a5a8498ad9";
    sha256 = "0wyma88p66z69vpi0n25nrpwrxwkrwl8fbifll5jv83q98qsg30k";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/zsh/site-functions
    cp functions/* $out/share/zsh/site-functions
  '';

  meta.platforms = platforms.all;
}
