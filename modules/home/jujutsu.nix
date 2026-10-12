{
  config,
  lib,
  perSystem,
  ...
}:

let
  inherit (lib)
    literalExpression
    mdDoc
    mkEnableOption
    mkIf
    mkOption
    optionalAttrs
    types
    ;
  cfg = config.modules.shell.jujutsu;
  gitCfg = config.modules.shell.git;
in
{
  options.modules.shell.jujutsu = {
    enable = mkEnableOption "Jujutsu";

    user = {
      name = mkOption {
        type = types.str;
        default = gitCfg.user.name;
        defaultText = literalExpression "config.modules.shell.git.user.name";
        description = mdDoc ''
          The name of the user.
        '';
      };

      email = mkOption {
        type = types.str;
        default = gitCfg.user.email;
        defaultText = literalExpression "config.modules.shell.git.user.email";
        description = mdDoc ''
          The email of the user.
        '';
      };
    };

    signing = {
      key = mkOption {
        type = types.nullOr types.str;
        default = gitCfg.signing.key;
        defaultText = literalExpression "config.modules.shell.git.signing.key";
        description = mdDoc ''
          The signing key to use for commits.
        '';
      };

      backend = mkOption {
        type = types.enum [
          "gpg"
          "ssh"
        ];
        default = if gitCfg.signing.format == "ssh" then "ssh" else "gpg";
        defaultText = literalExpression ''if config.modules.shell.git.signing.format == "ssh" then "ssh" else "gpg"'';
        description = mdDoc ''
          The signing backend to use (gpg or ssh).
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    programs.jujutsu = {
      enable = true;
      settings = {
        user = { inherit (cfg.user) email name; };
      }
      // optionalAttrs (cfg.signing.key != null) {
        signing = {
          behavior = "own";
          inherit (cfg.signing) backend key;
        };
      };
    };

    home.packages = [ perSystem.self.jj-hunk-tool ];

    home.shellAliases.rj = "rujutsu";
  };
}
