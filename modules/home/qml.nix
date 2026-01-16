{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.qml;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
  ];

  options.modules.dev.lang.qml = {
    enable = mkEnableOption "QML development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ qt6.qtdeclarative ];

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
