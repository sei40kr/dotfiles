{ config, lib, pkgs, ... }:

with lib;
let fontsEnabled = config.modules.desktop.fonts.enable;
in {
  options.modules.desktop.i18n.japanese.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.i18n.japanese.enable {
    modules.desktop = {
      config.fontconfig.defaultFonts = mkIf fontsEnabled {
        # FIXME do not override default option values
        sansSerif = [ "Noto Sans CJK JP" ];
        serif = [ "Noto Serif CJK JP" ];
        monospace = [ "Noto Sans Mono CJK JP" ];
      };
      tools.fcitx.enable = mkForce true;
    };

    my.packages = with pkgs; optionals fontsEnabled [ noto-fonts-cjk ];
  };
}
