{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.fonts;
in {
  options.modules.editors.fonts = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    fonts.fonts = with pkgs;
      with pkgs.my; [
        fira-code
        jetbrains-mono
        operator-mono
        source-code-pro
      ];
  };
}
