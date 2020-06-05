{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./zsh.nix ];
}
