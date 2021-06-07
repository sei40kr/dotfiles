{ fetchFromGitHub, lib, stdenv, ... }:

with lib;
stdenv.mkDerivation {
  pname = "zsh-smart-command-history";
  version = "unstable-2020-12-20";

  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "zsh-smart-command-history";
    rev = "e578f2757018dad7c6c76f45f7bb5d2c8e351f6f";
    sha256 = "1sjfig9c870l6q7f9ssc8qk1d8nmrpbzwmj6abkjcsb140cgqf0v";
  };

  dontBuild = true;

  installPhase = ''
    install -D smart-command-history.plugin.zsh \
      -T "''${out}/share/zsh/plugins/zsh-smart-command-history/smart-command-history.plugin.zsh"
  '';

  meta.platforms = platforms.all;
}
