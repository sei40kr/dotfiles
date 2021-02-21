{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.tools.jupyter;
in {
  options.modules.dev.tools.jupyter = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    package = mkOption {
      type = types.package;
      default = pkgs.python3.withPackages
        (ps: with ps; [ jupyter ipython matplotlib numpy pandas ]);
    };
  };

  config = mkIf cfg.enable { user.packages = [ cfg.package ]; };
}
