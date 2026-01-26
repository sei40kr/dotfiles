{
  config,
  lib,
  perSystem,
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

    package = mkPackageOption perSystem.anyrun "anyrun-with-all-plugins" {
      extraDescription = ''
        The Anyrun package to use.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      cfg.package
      anyrun-dmenu
      pkgs.rink
    ];

    xdg.configFile."anyrun/config.ron".source = pkgs.replaceVars ./config.ron {
      inherit (cfg) package;
    };
    xdg.configFile."anyrun/applications.ron".source = ./applications.ron;
    xdg.configFile."anyrun/style.css".source = ./style.css;

    systemd.user.services.anyrun = {
      Unit = {
        Description = "Anyrun daemon";
        PartOf = "graphical-session.target";
        After = "graphical-session.target";
      };
      Service = {
        Type = "simple";
        ExecStart = "${lib.getExe cfg.package} daemon";
        Restart = "on-failure";
        KillMode = "process";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
