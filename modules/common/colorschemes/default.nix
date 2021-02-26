{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.colorschemes;
in {
  options.modules.colorschemes = with types; {
    active = mkOpt str null;
    colors = {
      background = mkOpt str null;
      cursor = mkOpt str null;
      text = mkOpt str null;

      color0 = mkOpt str null;
      color1 = mkOpt str null;
      color2 = mkOpt str null;
      color3 = mkOpt str null;
      color4 = mkOpt str null;
      color5 = mkOpt str null;
      color6 = mkOpt str null;
      color7 = mkOpt str null;
      color8 = mkOpt str null;
      color9 = mkOpt str null;
      color10 = mkOpt str null;
      color11 = mkOpt str null;
      color12 = mkOpt str null;
      color13 = mkOpt str null;
      color14 = mkOpt str null;
      color15 = mkOpt str null;
    };
  };

  config.home-manager.users.${config.user.name}.programs.termite = {
    backgroundColor = cfg.colors.background;
    cursorColor = cfg.colors.cursor;
    foregroundColor = cfg.colors.text;

    colorsExtra = ''
      color0 = ${cfg.colors.color0}
      color1 = ${cfg.colors.color1}
      color2 = ${cfg.colors.color2}
      color3 = ${cfg.colors.color3}
      color4 = ${cfg.colors.color4}
      color5 = ${cfg.colors.color5}
      color6 = ${cfg.colors.color6}
      color7 = ${cfg.colors.color7}
      color8 = ${cfg.colors.color8}
      color9 = ${cfg.colors.color9}
      color10 = ${cfg.colors.color10}
      color11 = ${cfg.colors.color11}
      color12 = ${cfg.colors.color12}
      color13 = ${cfg.colors.color13}
      color14 = ${cfg.colors.color14}
      color15 = ${cfg.colors.color15}
    '';
  };
}
