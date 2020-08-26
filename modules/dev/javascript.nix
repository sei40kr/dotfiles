{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.javascript.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.javascript.enable {
    # TODO eslint-cli
    # TODO gatsby-cli
    # TODO prettier-eslint-cli
    # TODO tslint
    modules.dev.editors = {
      tools.packages = with pkgs;
        with pkgs.nodePackages; [
          create-react-app
          eslint_d
          gulp-cli
          javascript-typescript-langserver
          prettier
          typescript
          typescript-language-server
          webpack-cli
        ];
      vscodium.settings."[javascript]"."editor.defaultFormatter" =
        "esbenp.prettier-vscode";
    };
  };
}
