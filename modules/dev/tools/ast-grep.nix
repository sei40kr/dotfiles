{ config, lib, pkgs, ... }:

let
  inherit (lib) mdDoc mkEnableOption mkIf;
  cfg = config.modules.dev.tools.ast-grep;
in
{
  options.modules.dev.tools.ast-grep = {
    enable = mkEnableOption (mdDoc "ast-grep");
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ ast-grep ];
  };
}

