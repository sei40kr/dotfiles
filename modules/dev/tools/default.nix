{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./flexget.nix ./git.nix ./zeal.nix ];
}
