{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.dev.lang.go;
in
{
  options.modules.dev.lang.go = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
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
