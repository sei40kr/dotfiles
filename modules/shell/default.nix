{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./tmux.nix ./zsh.nix ];
}
