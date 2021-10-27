{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.shell.git;
in {
  options.modules.shell.git = with types; {
    enable = mkBoolOpt false;

    user = {
      name = mkOpt str "Seong Yong-ju";
      email = mkOpt str "sei40kr@gmail.com";
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ git gitAndTools.gitflow ];

    home.configFile."git/ignore".source = "${configDir}/git/ignore";

    home-manager.users.${config.user.name}.programs.git = {
      enable = true;
      delta.enable = true;
      includes = [{ path = "${configDir}/git/config"; }];
      userName = cfg.user.name;
      userEmail = cfg.user.email;
    };
  };
}
