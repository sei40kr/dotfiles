{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) binDir;
  cfg = config.modules.desktop;
in
{
  options.modules.desktop = with types; {
    enable = mkBoolOpt false;

    autoRepeat = {
      delay = mkOpt int 200;
      interval = mkOpt int 30;
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ my.wayland-sensible-terminal ];
  };
}
