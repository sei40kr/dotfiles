{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./emacs.nix ];
}
