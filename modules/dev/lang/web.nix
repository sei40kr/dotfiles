{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.lang.web;
in
{
  options.modules.dev.lang.web = {
    enable = mkBoolOpt false;

    bun.enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to install Bun.
      '';
    };
  };

  config = mkIf cfg.enable {
    # TODO stylelint-cli
    user.packages = with pkgs; [
      nodejs_20
      emmet-language-server
      nodePackages.prettier
      nodePackages.vscode-html-languageserver-bin
      nodePackages.vscode-css-languageserver-bin
      nodePackages.vue-language-server
    ] ++ (if cfg.bun.enable then [ bun ] else [ ]);
  };
}
