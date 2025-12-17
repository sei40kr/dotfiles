{
  lib,
  config,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf mkPackageOption;
  cfg = config.modules.desktop.apps.anyrun;

  anyrun-dmenu = pkgs.writeShellScriptBin "anyrun-dmenu" ''
    exec ${cfg.package}/bin/anyrun \
      --plugins ${cfg.package}/lib/libstdin.so \
      --show-results-immediately true \
      --hide-plugin-info true \
      "$@"
  '';
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
      anyrun-dmenu
      rink
    ];

    home.configFile."anyrun/config.ron".source = pkgs.replaceVars ../../../config/anyrun/config.ron {
      inherit (cfg) package;
    };
    home.configFile."anyrun/applications.ron".source = ../../../config/anyrun/applications.ron;
    home.configFile."anyrun/style.css".source = ../../../config/anyrun/style.css;
  };
}
