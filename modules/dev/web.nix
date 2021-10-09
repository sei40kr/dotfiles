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
  };
}
