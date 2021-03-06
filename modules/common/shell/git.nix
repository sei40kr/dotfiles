{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.git;
in {
  options.modules.shell.git = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ git gitAndTools.gitflow ];
    home.configFile."git/ignore".source = "${configDir}/git/ignore";
    home-manager.users.${config.user.name}.programs.git = {
      enable = true;
      delta.enable = true;
      includes = [{ path = "${configDir}/git/config"; }];
      userName = "Seong Yong-ju";
      userEmail = "sei40kr@gmail.com";
    };
    modules.shell.zsh.zinitPluginsInit = ''
      zinit snippet OMZP::git/git.plugin.zsh
      zinit snippet OMZP::git-flow/git-flow.plugin.zsh
    '';
  };
}
