{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./tools ./tmux.nix ./zsh.nix ];
}
