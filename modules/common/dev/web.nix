{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.web;
  nvm = builtins.fetchGit {
    url = "https://github.com/nvm-sh/nvm.git";
    rev = "258938ef66a2a49a4a400554a6dce890226ae34c"; # v0.35.3
  };
  nvmRootFiles = [ "bash_completion" "nvm-exec" "nvm.sh" ];
in {
  options.modules.dev.web = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO stylelint-cli
    user.packages = with pkgs; [
      nodejs
      yarn
      nodePackages.vscode-html-languageserver-bin
      nodePackages.vscode-css-languageserver-bin
      nodePackages.vue-language-server
    ];
    home.file = foldl (files: name:
      files // {
        ".nvm/${name}".source = "${nvm.outPath}/${name}";
      }) { } nvmRootFiles;
    env = {
      NVM_DIR = [ "\${HOME}/.nvm" ];
      PATH = [ "\${HOME}/.yarn/bin" ];
    };
    modules.shell.zsh = {
      aliases = {
        npmg = "npm i -g ";
        npmS = "npm i -S ";
        npmD = "npm i -D ";
        npmE = ''PATH="$(npm bin):''${PATH}"'';
        npmO = "npm outdated";
        npmV = "npm -v";
        npmL = "npm list";
        npmL0 = "npm ls --depth=0";
        npmst = "npm start";
        npmt = "npm test";
        npmR = "npm run";
        npmP = "npm publish";
        npmI = "npm init";
      };
      zinitPluginsInit = ''
        zinit light ${pkgs.my.zshPlugins.lazy-nvm}/share/zsh/plugins/lazy-nvm
        zinit snippet OMZP::yarn/yarn.plugin.zsh
      '';
    };
  };
}
