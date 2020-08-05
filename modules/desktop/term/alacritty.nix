{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.term.alacritty.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.term.alacritty.enable {
    modules.shell.tmux.extraConfig = ''
      if-shell '[ "$TERM" = alacritty ]' {
          set-option -as terminal-overrides ,alacritty:RGB
          set-option -g set-titles-string 'Alacritty@#S'
      }
    '';

    my.packages = with pkgs; [ alacritty ];
    my.env.TERMINAL = "${pkgs.alacritty}/bin/alacritty";
    my.home.xdg.configFile."alacritty/alacritty.yml".source =
      <config/alacritty/alacritty.yml>;
  };
}
