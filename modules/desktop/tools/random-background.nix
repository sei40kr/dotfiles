{ config, lib, options, pkgs, ... }:

with lib;
(let cfg = config.modules.desktop.tools.randomBackground;
in {
  options.modules.desktop.tools.randomBackground = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    imageDirectory = mkOption { type = types.str; };
  };

  config = mkIf cfg.enable {
    my.home.services.random-background = {
      enable = true;
      imageDirectory = cfg.imageDirectory;
    };
  };
})
