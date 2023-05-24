{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.raycast;
in
{
  options.modules.desktop.apps.raycast = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # Install Raycast via Homebrew Cask because it checks for updates every time
    # it starts, and it's difficult to keep it up to date with Nix.
    homebrew = {
      enable = true;
      casks = [ "raycast" ];
    };
  };
}
