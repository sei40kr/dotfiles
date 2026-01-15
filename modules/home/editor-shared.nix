{
  lib,
  inputs,
  config,
  pkgs,
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
    listOf
    nullOr
    package
    str
    submodule
    ;
  inherit (inputs.self.lib.extraTypes) fontType;

  cfg = config.modules.editors;

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
  options.modules.editors = {
    defaultEditor = mkOption {
      type = nullOr str;
      readOnly = true;
      default =
        if cfg.lazyvim.enable then
          "lazyvim"
        else if cfg.emacs.enable then
          "emacs"
        else if cfg.helix.enable then
          "hx"
        else
          null;
      description = "The default editor to use, automatically determined by enabled editor modules";
    };

    fonts = {
      code = mkOption {
        type = fontType;
        default = {
          name = "monospace";
          size = 12;
        };
        description = "Code font configuration";
      };

      ui = mkOption {
        type = fontType;
        default = {
          name = "sans-serif";
          size = 11;
        };
        description = "UI font configuration";
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
    home.packages =
      with pkgs;
      [
        anonymousPro
        fantasque-sans-mono
        fira-code
        hack-font
        hasklig
        inconsolata
        # input-fonts
        jetbrains-mono
        source-code-pro
        source-han-code-jp
        ubuntu-classic
        victor-mono
        (mkIf (cfg.fonts.code.package != null) cfg.fonts.code.package)
        (mkIf (cfg.fonts.ui.package != null) cfg.fonts.ui.package)
      ]
      ++ (lib.flatten (
        lib.mapAttrsToList (
          name: server: lib.optional (server.package != null) server.package
        ) cfg.lspServers
      ));

    home.sessionVariables = mkIf (cfg.defaultEditor != null) {
      EDITOR = cfg.defaultEditor;
    };
  };
}
