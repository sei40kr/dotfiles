{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./tools ./bat.nix ./exa.nix ./tmux.nix ./zsh.nix ];
}
