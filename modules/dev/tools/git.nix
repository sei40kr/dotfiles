{ config, lib, pkgs, ... }:

with lib;
(let
  cfg = config.modules.dev.tools.git;
  gitConfig = {
    user = {
      email = config.my.userEmail;
      name = config.my.userFullName;
    };
  };
  deltaConfig = {
    core.pager = "${pkgs.my.delta}/bin/delta";
    interactive.diffFilter = "${pkgs.my.delta}/bin/delta --color-only";
    delta = {
      syntax-theme = config.modules.shell.tools.bat.theme;
      line-numbers = true;
    };
  };
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

    enableDelta = mkOption {
      type = types.bool;
      default = true;
    };
  };

  config = mkIf config.modules.dev.tools.git.enable {
    modules.shell = {
      tools.bat.enable = true;
      zsh.zinitPluginsInit = ''
        zinit snippet OMZP::git/git.plugin.zsh
      '' + optionalString cfg.enableGitFlow ''
        zinit snippet OMZP::git-flow/git-flow.plugin.zsh
      '';
    };

    my.packages = with pkgs;
      with pkgs.my;
      ([ git ] ++ optionals cfg.enableGitFlow [ gitAndTools.gitflow ]
        ++ optionals cfg.enableGitCrypt [ git-crypt ]
        ++ optionals cfg.enableDelta [ delta ]);

    my.home.xdg.configFile = {
      "git/config".text = generators.toINI { } ({
        include.path = "${<config/git/config>}";
      } // gitConfig // optionalAttrs cfg.enableDelta deltaConfig);
      "git/ignore".source = <config/git/ignore>;
    };
  };
})
