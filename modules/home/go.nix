{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.go;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
  ];

  options.modules.dev.lang.go = {
    enable = mkEnableOption "Go development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      go
      gore
    ];

    modules.editors.lspServers.gopls = rec {
      package = pkgs.gopls;
      command = "${package}/bin/gopls";
      filetypes = [
        "go"
        "gomod"
        "gowork"
        "gotmpl"
      ];
      rootMarkers = [
        "go.mod"
        "go.work"
        ".git"
      ];
    };
  };
}
