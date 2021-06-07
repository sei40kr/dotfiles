{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.web;
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
    env = { PATH = [ "\${HOME}/.yarn/bin" ]; };
    modules.shell = {
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

      zsh.zinit.snippets = [{
        source =
          "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/yarn/yarn.plugin.zsh";
        ice.id-as = "OMZP::yarn";
      }];
    };
  };
}
