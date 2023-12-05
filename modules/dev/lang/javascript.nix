{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.lang.javascript;
in
{
  options.modules.dev.lang.javascript = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # TODO gatsby-cli
    # TODO prettier-eslint-cli
    user.packages = with pkgs; [
      nodePackages.create-react-app
      biome
      nodePackages.eslint
      nodePackages.eslint_d
      nodePackages.typescript
      nodePackages.typescript-language-server
      nodePackages.vue-language-server
      nodePackages.webpack-cli
    ];
  };
}
