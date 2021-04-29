{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.fonts;
in {
  options.modules.editors.fonts = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    fonts.fonts = with pkgs; [
      fira-code
      jetbrains-mono
      my.operator-mono
      source-code-pro
    ];
  };
}
