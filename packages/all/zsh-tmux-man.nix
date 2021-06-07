{ fetchFromGitHub, lib, stdenv, ... }:

with lib;
stdenv.mkDerivation {
  pname = "zsh-tmux-man";
  version = "unstable-2021-01-24";

  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "zsh-tmux-man";
    rev = "41cbc1291e6e0bc6d1a1d0cb0cf48e227611968e";
    sha256 = "13nb75lv4y4aragd6arifc8avswaykk88dp52g5qcfcpywnr18nr";
  };

  dontBuild = true;

  installPhase = ''
    install -D tmux-man.plugin.zsh \
      -T "''${out}/share/zsh/plugins/zsh-tmux-man/tmux-man.plugin.zsh"
  '';

  meta.platforms = platforms.all;
}
