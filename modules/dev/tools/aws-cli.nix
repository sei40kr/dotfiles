{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.tools.awsCli;
in {
  options.modules.dev.tools.awsCli = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    credentials = mkOption {
      type = with types;
        attrsOf (submodule {
          options = {
            accessKeyId = mkOption { type = str; };
            secretAccessKey = mkOption { type = str; };
          };
        });
      default = { };
    };
  };

  config = mkIf cfg.enable {
    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice wait''' lucid
      zinit snippet ${pkgs.awscli}/share/zsh/site-functions/aws_zsh_completer.sh
    '';

    user.packages = with pkgs; [ awscli ];

    home.file.".aws/credentials".text = mkIf (cfg.credentials != { })
      (generators.toINI { } (mapAttrs (k: v: {
        aws_access_key_id = v.accessKeyId;
        aws_secret_access_key = v.secretAccessKey;
      }) cfg.credentials));
  };
}
