{
  config,
  lib,
  ...
}:

let
  inherit (lib)
    literalExpression
    mkIf
    mkOption
    types
    ;
  inherit (types)
    attrs
    attrsOf
    int
    listOf
    nullOr
    package
    str
    submodule
    ;
  inherit (lib.my) mkOpt;

  cfg = config.modules.editors;

  fontType = submodule {
    options = {
      package = mkOpt (nullOr package) null;
      name = mkOpt str null;
      size = mkOpt int null;
    };
  };

  lspServerType = submodule {
    options = {
      package = mkOption {
        type = nullOr package;
        default = null;
        description = "The package providing the LSP server";
        example = literalExpression "pkgs.rust-analyzer";
      };

      command = mkOption {
        type = str;
        description = "Command to run the LSP server";
        example = "rust-analyzer";
      };

      args = mkOption {
        type = listOf str;
        default = [ ];
        description = "Arguments to pass to the LSP server command";
        example = [ "--stdio" ];
      };

      filetypes = mkOption {
        type = listOf str;
        default = [ ];
        description = "List of file types the LSP server should attach to";
        example = [ "rust" ];
      };

      rootMarkers = mkOption {
        type = listOf str;
        default = [ ".git" ];
        description = "Files or directories that mark the root of a project";
        example = [
          "Cargo.toml"
          ".git"
        ];
      };

      initOptions = mkOption {
        type = attrs;
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
        type = attrs;
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
  imports = [
    ./datagrip.nix
    ./dataspell.nix
    ./emacs.nix
    ./fonts.nix
    ./helix.nix
    ./idea.nix
    ./ideavim.nix
    ./lazyvim.nix
    ./zed.nix
  ];

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
      type = attrsOf lspServerType;
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
