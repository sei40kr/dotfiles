{ config, lib, options, pkgs, ... }:

with lib;
(let cfg = config.modules.dev.tools.git;
in {
  options.modules.dev.tools.git.enable = mkOption {
    type = types.bool;
    default = false;
  };

  options.modules.dev.tools.git.enableGitFlow = mkOption {
    type = types.bool;
    default = false;
  };

  options.modules.dev.tools.git.enableGitCrypt = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.git.enable {
    my.packages = with pkgs;
      ([ git ] ++ optionals cfg.enableGitFlow [ gitAndTools.gitflow ]
        ++ optionals cfg.enableGitCrypt [ git-crypt ]);

    my.home.xdg.configFile = {
      "git/config".source = <config/git/config>;
      "git/ignore".source = <config/git/ignore>;
    };

    modules.shell.zsh.zinitPluginsInit = ''
      zinit snippet OMZP::git/git.plugin.zsh
    '' + optionals cfg.enableGitFlow ''
      zinit snippet OMZP::git-flow/git-flow.plugin.zsh
    '';
  };
})
