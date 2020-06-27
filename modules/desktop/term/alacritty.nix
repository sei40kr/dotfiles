{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.term.alacritty.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.term.alacritty.enable {
    my.packages = with pkgs; [ alacritty ];
    my.env.TERMINAL = "${pkgs.alacritty}/bin/alacritty";
    my.home.xdg.configFile."alacritty/alacritty.yml".source =
      <config/alacritty/alacritty.yml>;
  };
}
