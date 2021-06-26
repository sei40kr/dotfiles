{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.fontconfig;
in {
  options.modules.desktop.fontconfig.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    fonts = {
      fontconfig = {
        enable = true;
        defaultFonts = {
          emoji = [ "Noto Color Emoji" ];
          monospace = [ "Noto Sans Mono" "Noto Sans Mono CJK JP" ];
          sansSerif = [ "Noto Sans" "Noto Sans CJK JP" ];
          serif = [ "Noto Serif" "Noto Serif CJK JP" ];
        };
        hinting.enable = false;
      };

      fonts = with pkgs; [ noto-fonts noto-fonts-cjk noto-fonts-emoji ];
    };

    modules.desktop.dconf = mkIf config.modules.desktop.gnome.enable {
      enable = true;
      settings."org/gnome/desktop/interface" = {
        font-antialiasing = "rgba";
        font-hinting = "none";
        font-rgba-order = "rgb";
      };
    };
  };
}
