{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.editors.fonts;
in {
  options.modules.dev.editors.fonts = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    packages = mkOption {
      type = with types; listOf package;
      default = with pkgs;
        with pkgs.my; [
          fira-code
          jetbrains-mono
          operator-mono
          source-code-pro
        ];
    };
  };

  config = mkIf cfg.enable { my.packages = cfg.packages; };
}
