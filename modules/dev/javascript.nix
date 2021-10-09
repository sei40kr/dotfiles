{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.javascript;
in {
  options.modules.dev.javascript = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO gatsby-cli
    # TODO prettier-eslint-cli
    user.packages = with pkgs; [
      nodePackages.create-react-app
      nodePackages.eslint
      nodePackages.eslint_d
      nodePackages.javascript-typescript-langserver
      nodePackages.prettier
      nodePackages.typescript
      nodePackages.typescript-language-server
      nodePackages.webpack-cli
    ];
  };
}
