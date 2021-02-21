{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
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

    user.packages = with pkgs; [ alacritty ];
    env.TERMINAL = "${pkgs.alacritty}/bin/alacritty";
    home.configFile."alacritty/alacritty.yml".source =
      "${configDir}/alacritty/alacritty.yml";
  };
}
