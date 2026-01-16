{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.lean;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
  ];

  options.modules.dev.lang.lean = {
    enable = mkEnableOption "Lean development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ lean4 ];

    modules.editors.lspServers.lean4ls = rec {
      package = pkgs.lean4;
      command = "${package}/bin/lean";
      args = [ "--server" ];
      filetypes = [ "lean" ];
      rootMarkers = [
        "lean-toolchain"
        "lakefile.lean"
        ".git"
      ];
    };
  };
}
