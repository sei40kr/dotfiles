{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.dev.lang.nix;
in
{
  options.modules.dev.lang.nix = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      nix-init
      nix-melt
      nurl
      nixfmt-rfc-style
      unstable.mcp-nixos
      nil
    ];

    # Configure NixOS MCP server
    modules.ai.mcpServers = {
      nixos = rec {
        transport = "stdio";
        package = pkgs.unstable.mcp-nixos;
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
