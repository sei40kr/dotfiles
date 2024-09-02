{ config, lib, pkgs, ... }:

let
  inherit (lib) generators mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.polybar;
in
{
  options.modules.desktop.apps.polybar = {
    enable = mkEnableOption "Polybar";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ polybarFull ];

    fonts.packages = with pkgs; [
      (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })
    ];

    systemd.user.services.polybar-top = {
      description = "Polybar top bar as systemd service";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      restartTriggers = [ ../../../config/polybar/config.ini ];
      script = "${pkgs.polybarFull}/bin/polybar top";
    };
    systemd.user.services.polybar-bottom = {
      description = "Polybar bottom bar as systemd service";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      restartTriggers = [ ../../../config/polybar/config.ini ];
      script = "${pkgs.polybarFull}/bin/polybar bottom";
    };

    environment.etc."xdg/polybar/config.ini".text = generators.toINIWithGlobalSection { } {
      globalSection = {
        include-file = "${../../../config/polybar/config.ini}";
      };
    };
  };
}
