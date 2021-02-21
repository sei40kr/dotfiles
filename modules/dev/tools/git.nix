{ config, lib, pkgs, ... }:

with lib;
with lib.my;
(let
  cfg = config.modules.dev.tools.git;
  gitConfig = {
    user = {
      email = "sei40kr@gmail.com";
      name = "Seong Yong-ju";
    };
  };
  deltaConfig = {
    core.pager = "${pkgs.delta}/bin/delta";
    interactive.diffFilter = "${pkgs.delta}/bin/delta --color-only";
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
      tools.bat.enable = lib.mkForce true;
      zsh.zinitPluginsInit = ''
        zinit snippet OMZP::git/git.plugin.zsh
      '' + optionalString cfg.enableGitFlow ''
        zinit snippet OMZP::git-flow/git-flow.plugin.zsh
      '';
    };

    user.packages = with pkgs;
      ([ git ] ++ optionals cfg.enableGitFlow [ gitAndTools.gitflow ]
        ++ optionals cfg.enableGitCrypt [ git-crypt ]
        ++ optionals cfg.enableDelta [ gitAndTools.delta ]);

    home.configFile = {
      "git/config".text = generators.toINI { } ({
        include.path = "${configDir}/git/config";
      } // gitConfig // optionalAttrs cfg.enableDelta deltaConfig);
      "git/ignore".source = "${configDir}/git/ignore";
    };
  };
})
