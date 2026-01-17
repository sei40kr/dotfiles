{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.shell;
in
{
  options.modules.dev.lang.shell = {
    enable = mkEnableOption "Shell script development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      shellcheck
      shfmt
    ];

    modules.editors.lspServers.bashls = rec {
      package = pkgs.nodePackages.bash-language-server;
      command = "${package}/bin/bash-language-server";
      args = [ "start" ];
      filetypes = [
        "sh"
        "bash"
      ];
      rootMarkers = [ ".git" ];
      settings = {
        bashIde = {
          globPattern = "*@(.sh|.inc|.bash|.command)";
        };
      };
    };
  };
}
