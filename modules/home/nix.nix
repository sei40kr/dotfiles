{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.nix;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
    inputs.self.homeModules.ai-shared
  ];

  options.modules.dev.lang.nix = {
    enable = mkEnableOption "Nix development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      nix-init
      nix-melt
      nurl
      nixfmt-rfc-style
      mcp-nixos
      nil
      statix
    ];

    modules.ai.mcpServers = {
      nixos = rec {
        transport = "stdio";
        package = pkgs.mcp-nixos;
        command = "${package}/bin/mcp-nixos";
        args = [ ];
      };
    };

    modules.editors.lspServers.nil_ls = rec {
      package = pkgs.nil;
      command = "${package}/bin/nil";
      filetypes = [ "nix" ];
      rootMarkers = [
        "flake.nix"
        ".git"
      ];
    };
  };
}
