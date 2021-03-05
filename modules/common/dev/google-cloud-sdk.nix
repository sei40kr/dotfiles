{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.google-cloud-sdk;
  package = pkgs.google-cloud-sdk;
in {
  options.modules.dev.google-cloud-sdk.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf cfg.enable {
    user.packages = [ package ];
    env.CLOUDSDK_ROOT_DIR = "${package}/google-cloud-sdk";
    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice wait'''
      zinit snippet "''${CLOUDSDK_ROOT_DIR}/completion.zsh.inc"
    '';
  };
}
