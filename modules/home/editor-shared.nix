{
  config,
  lib,
  ...
}:

let
  inherit (lib)
    literalExpression
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
  options.modules.editors.lspServers = mkOption {
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
}
