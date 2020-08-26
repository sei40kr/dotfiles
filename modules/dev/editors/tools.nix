{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.editors.tools;
in {
  options.modules.dev.editors.tools = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    packages = mkOption {
      type = with types; listOf package;
      default = [ ];
    };
  };

  config = mkIf cfg.enable { my.packages = cfg.packages; };
}
