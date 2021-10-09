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

    # TODO install completion in google-cloud-sdk derivation
    modules.shell.zsh.rcInit = ''
      zinit ice wait''' lucid
      zinit snippet ${package}/google-cloud-sdk/completion.zsh.inc
    '';
  };
}
