{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.shell.git;
in
{
  options.modules.shell.git = with types; {
    enable = mkBoolOpt false;

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
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ git gitAndTools.gitflow gitui ];

    home.configFile."git/ignore".source = "${configDir}/git/ignore";

    home-manager.users.${config.user.name}.programs.git = {
      enable = true;
      delta.enable = true;
      includes = [{ path = "${configDir}/git/config"; }];
      userName = cfg.user.name;
      userEmail = cfg.user.email;
    };

    home.configFile."gitui/key_bindings.ron".source = "${configDir}/gitui/key_bindings.ron";
  };
}
