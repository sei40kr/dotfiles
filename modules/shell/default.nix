{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./bat.nix ./exa.nix ./tmux.nix ./zsh.nix ];
}
