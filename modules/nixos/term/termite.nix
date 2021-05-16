{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.term.termite;
  colors = config.modules.term.colorschemes.colors;
in {
  options.modules.desktop.term.termite = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.termite = {
      enable = true;
      font = "Monospace 12";

      backgroundColor = colors.background;
      foregroundColor = colors.foreground;
      cursorColor = colors.cursor;
      colorsExtra = ''
        color0 = ${colors.normal.black}
        color1 = ${colors.normal.red}
        color2 = ${colors.normal.green}
        color3 = ${colors.normal.yellow}
        color4 = ${colors.normal.blue}
        color5 = ${colors.normal.magenta}
        color6 = ${colors.normal.cyan}
        color7 = ${colors.normal.white}
        color8 = ${colors.bright.black}
        color9 = ${colors.bright.red}
        color10 = ${colors.bright.green}
        color11 = ${colors.bright.yellow}
        color12 = ${colors.bright.blue}
        color13 = ${colors.bright.magenta}
        color14 = ${colors.bright.cyan}
        color15 = ${colors.bright.white}
      '';
    };
    modules.desktop.term.terminal = "${pkgs.termite}/bin/termite";
  };
}
