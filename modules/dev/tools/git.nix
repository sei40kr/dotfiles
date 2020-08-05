{ config, lib, pkgs, ... }:

with lib;
(let cfg = config.modules.dev.tools.git;
in {
  options.modules.dev.tools.git = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enableGitFlow = mkOption {
      type = types.bool;
      default = false;
    };

    enableGitCrypt = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.dev.tools.git.enable {
    my.packages = with pkgs;
      ([ git ] ++ optionals cfg.enableGitFlow [ gitAndTools.gitflow ]
        ++ optionals cfg.enableGitCrypt [ git-crypt ]);

    my.home.xdg.configFile = {
      "git/config".text = ''
        [include]
          path = ${<config/git/config>}

        [user]
          email = ${config.my.userEmail}
          name = ${config.my.userFullName}
      '';
      "git/ignore".source = <config/git/ignore>;
    };

    modules.shell.zsh.zinitPluginsInit = ''
      zinit snippet OMZP::git/git.plugin.zsh
    '' + optionals cfg.enableGitFlow ''
      zinit snippet OMZP::git-flow/git-flow.plugin.zsh
    '';
  };
})
