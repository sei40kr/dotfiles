{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.editors;

  fontType = types.submodule {
    options = {
      package = mkOpt (types.nullOr types.package) null;
      name = mkOpt types.str null;
      size = mkOpt types.int null;
    };
  };

  lspServerType = types.submodule {
    options = {
      package = mkOption {
        type = types.nullOr types.package;
        default = null;
        description = "The package providing the LSP server";
        example = literalExpression "pkgs.rust-analyzer";
      };

      command = mkOption {
        type = types.str;
        description = "Command to run the LSP server";
        example = "rust-analyzer";
      };

      args = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Arguments to pass to the LSP server command";
        example = [ "--stdio" ];
      };

      filetypes = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "List of file types the LSP server should attach to";
        example = [ "rust" ];
      };

      rootMarkers = mkOption {
        type = types.listOf types.str;
        default = [ ".git" ];
        description = "Files or directories that mark the root of a project";
        example = [
          "Cargo.toml"
          ".git"
        ];
      };

      initOptions = mkOption {
        type = types.attrs;
        default = { };
        description = "Initialization options passed to the LSP server";
        example = literalExpression ''
          {
            cargo = {
              features = "all";
            };
          }
        '';
      };

      settings = mkOption {
        type = types.attrs;
        default = { };
        description = "Settings for the LSP server";
        example = literalExpression ''
          {
            "rust-analyzer.cargo.check.command" = "clippy";
          }
        '';
      };
    };
  };
in
{
  options.modules.editors = {
    fonts = {
      code = mkOpt fontType {
        name = "monospace";
        size = 12;
      };
      ui = mkOpt fontType {
        name = "sans-serif";
        size = 11;
      };
    };

    lspServers = mkOption {
      type = types.attrsOf lspServerType;
      default = { };
      description = "LSP server configurations";
      example = literalExpression ''
        {
          rust-analyzer = {
            package = pkgs.rust-analyzer;
            command = "rust-analyzer";
            args = [ ];
            filetypes = [ "rust" ];
            settings = {
              "rust-analyzer.cargo.check.command" = "clippy";
            };
          };
        }
      '';
    };
  };

  config = {
    fonts.packages = [
      (mkIf (cfg.fonts.code.package != null) cfg.fonts.code.package)
      (mkIf (cfg.fonts.ui.package != null) cfg.fonts.ui.package)
    ];

    environment.systemPackages = lib.flatten (
      lib.mapAttrsToList (
        name: server: lib.optional (server.package != null) server.package
      ) cfg.lspServers
    );
  };
}
