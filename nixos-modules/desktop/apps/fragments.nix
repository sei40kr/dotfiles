{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.fragments;
in {
  options.modules.desktop.apps.fragments = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ fragments ];

    modules.desktop.dconf = {
      enable = true;
      settings = {
        "de/haeckerfelix/Fragments" = {
          # Force encryption
          encryption-mode = 2;
        };
      };
    };
  };
}
