{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.hyprlang;
in
{
  options.modules.dev.lang.hyprlang = {
    enable = mkEnableOption "hyprlang";
  };

  config = mkIf cfg.enable {
    modules.editors.lspServers.hyprls = rec {
      package = pkgs.hyprls;
      command = "${package}/bin/hyprls";
      args = [ "--stdio" ];
      filetypes = [ "hyprlang" ];
      rootMarkers = [ ".git" ];
    };
  };
}
