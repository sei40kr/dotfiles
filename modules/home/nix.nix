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
        allowedTools = [
          "nixos_search"
          "nixos_info"
          "nixos_channels"
          "nixos_stats"
          "nixos_flakes_search"
          "nixos_flakes_stats"
          "home_manager_search"
          "home_manager_info"
          "home_manager_stats"
          "home_manager_list_options"
          "home_manager_options_by_prefix"
          "darwin_search"
          "darwin_info"
          "darwin_stats"
          "darwin_list_options"
          "darwin_options_by_prefix"
          "nixhub_package_versions"
          "nixhub_find_version"
        ];
      };
    };

    modules.ai.permissions.allowedCommandPrefixes = [
      "nix eval"
      "nix flake show"
      "nix flake metadata"
      "nix search"
      "nix path-info"
      "nixfmt"
      "statix"
    ];
    modules.ai.permissions.allowedFetchDomains = [
      "nixos.org"
      "wiki.nixos.org"
      "nix.dev"
    ];

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
