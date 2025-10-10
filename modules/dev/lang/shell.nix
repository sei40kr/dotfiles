{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.dev.lang.shell;
in
{
  options.modules.dev.lang.shell = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
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
