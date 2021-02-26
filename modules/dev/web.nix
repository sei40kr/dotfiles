{ config, lib, pkgs, ... }:

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
    modules = {
      # TODO stylelint-cli
      dev.editors.tools.packages = with pkgs.nodePackages; [
        vscode-html-languageserver-bin
        vscode-css-languageserver-bin
        vue-language-server
      ];
      shell.zsh.zinitPluginsInit = ''
        zinit light ${pkgs.my.zshPlugins.lazy-nvm}/share/zsh/plugins/lazy-nvm

        zinit snippet OMZP::yarn/yarn.plugin.zsh
      '';
    };

    home.file = foldl (files: name:
      files // {
        ".nvm/${name}".source = "${nvm.outPath}/${name}";
      }) { } nvmRootFiles;

    user.packages = with pkgs; [ nodejs yarn ];
    env = {
      NVM_DIR = [ "\${HOME}/.nvm" ];
      PATH = [ "\${HOME}/.yarn/bin" ];
    };
    modules.shell.zsh.aliases = {
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
  };
})
