{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.dev.lang.qml;
in
{
  options.modules.dev.lang.qml = {
    enable = mkEnableOption "QML language support";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ qt6.qtdeclarative ];

    # Necessary to set QML2_IMPORT_PATH
    qt.enable = true;

    modules.editors.lspServers.qmlls = rec {
      package = pkgs.qt6.qtdeclarative;
      command = "${package}/bin/qmlls";
      filetypes = [
        "qml"
        "qmljs"
      ];
      rootMarkers = [ ".git" ];
    };
  };
}
