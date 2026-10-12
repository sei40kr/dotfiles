{
  config,
  lib,
  perSystem,
  ...
}:

let
  inherit (lib)
    mdDoc
    mkEnableOption
    mkIf
    mkOption
    optionalAttrs
    types
    ;
  cfg = config.modules.shell.jujutsu;
in
{
  options.modules.shell.jujutsu = {
    enable = mkEnableOption "Jujutsu";

    user = {
      name = mkOption {
        type = types.str;
        default = "Seong Yong-ju";
        description = mdDoc ''
          The name of the user.
        '';
      };

      email = mkOption {
        type = types.str;
        default = "sei40kr@gmail.com";
        description = mdDoc ''
          The email of the user.
        '';
      };
    };

    signing = {
      key = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = mdDoc ''
          The signing key to use for commits.
        '';
      };

      backend = mkOption {
        type = types.enum [
          "gpg"
          "ssh"
        ];
        default = "gpg";
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
  };
}
