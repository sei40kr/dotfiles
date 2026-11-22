{
  config,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.quint;
in
{
  options.modules.dev.lang.quint = {
    enable = mkEnableOption "Quint development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ quint ];

    modules.editors.lspServers.quint_language_server = rec {
      package = perSystem.self.quint-language-server;
      command = "${package}/bin/quint-language-server";
      args = [ "--stdio" ];
      filetypes = [ "quint" ];
    };
  };
}
