{ config, lib, options, pkgs, ... }:

with lib;
(let
  nvm = builtins.fetchGit {
    url = "https://github.com/nvm-sh/nvm.git";
    rev = "258938ef66a2a49a4a400554a6dce890226ae34c"; # v0.35.3
  };
  nvmRootFiles = [ "bash_completion" "nvm-exec" "nvm.sh" ];
in {
  options.modules.dev.web.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.web.enable {
    my.home.home.file = foldl (files: name:
      files // {
        ".nvm/${name}".source = "${nvm.outPath}/${name}";
      }) { } nvmRootFiles;

    # TODO Install gulp, webpack-cli
    # TODO Install stylelint-cli, eslint-cli, eslint_d, and tslint
    # TODO Install prettier, prettier-eslint-cli, and typescript-formatter
    # TODO Install language servers for HTML, CSS, JavaScript, TypeScript, Vue
    my.packages = with pkgs; [ nodejs yarn ];

    my.env = {
      NVM_DIR = [ "\${HOME}/.nvm" ];
      PATH = [ "\${HOME}/.yarn/bin" ];
    };

    modules.shell.zsh.zinitPluginsInit = ''
      zinit light sei40kr/zsh-lazy-nvm

      zinit snippet OMZP::yarn/yarn.plugin.zsh
    '';
  };
})
