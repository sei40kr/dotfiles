{
  lib,
  config,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf mkPackageOption;
  cfg = config.modules.desktop.apps.anyrun;
in
{
  options.modules.desktop.apps.anyrun = {
    enable = mkEnableOption "Anyrun";

    package = mkPackageOption pkgs "anyrun" {
      extraDescription = ''
        The Anyrun package to use.
      '';
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      cfg.package
      rink
    ];

    home.configFile."anyrun/config.ron".source = pkgs.replaceVars ../../../config/anyrun/config.ron {
      inherit (cfg) package;
    };
    home.configFile."anyrun/applications.ron".source = ../../../config/anyrun/applications.ron;
    home.configFile."anyrun/style.css".source = ../../../config/anyrun/style.css;
  };
}
