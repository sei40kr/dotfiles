{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.web.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.web.enable
    (let nvm = builtins.fetchGit { url = "https://github.com/nvm-sh/nvm.git"; };
    in {
      # TODO Install gulp, webpack-cli
      # TODO Install stylelint-cli, eslint-cli, eslint_d, and tslint
      # TODO Install prettier, prettier-eslint-cli, and typescript-formatter
      # TODO Install language servers for HTML, CSS, JavaScript, TypeScript, Vue
      my.packages = with pkgs; [ nodejs yarn ];
    });
}
