{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.term.alacritty.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.term.alacritty.enable {
    my.packages = [ pkgs.alacritty ];

    my.home.xdg.configFile."alacritty/alacritty.yml".source =
      <config/alacritty/alacritty.yml>;
  };
}
