{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.theme;
in
{
  config = mkIf (cfg.active == "whitesur") {
    modules.desktop.gnome.shell.theme = {
      package = pkgs.whitesur-gtk-theme;
      name = "WhiteSur-light";
    };

    modules.desktop.gtk = {
      theme = {
        package = pkgs.whitesur-gtk-theme;
        name = "WhiteSur-light";
      };
      iconTheme = {
        package = pkgs.whitesur-icon-theme;
        name = "WhiteSur";
      };
    };
  };
}
