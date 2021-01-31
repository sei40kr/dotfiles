{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.tools.kaggle;
in {
  options.modules.dev.tools.kaggle = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    credential = mkOption {
      type = with types;
        nullOr (submodule {
          options = {
            userName = mkOption { type = types.str; };
            key = mkOption { type = types.str; };
          };
        });
      default = null;
    };
  };

  config = mkIf cfg.enable {
    my.packages = with pkgs; [ kaggle ];

    my.home.home.file.".kaggle/kaggle.json".text =
      mkIf (cfg.credential != null) builtins.toJSON {
        username = cfg.credential.userName;
        key = cfg.credential.key;
      };
  };
}
