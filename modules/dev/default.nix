{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./editors ./cc.nix ./git.nix ./kotlin.nix ./python.nix ];
}
