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

    modules.shell.zsh.zinit.snippets = [{
      source = "${package}/google-cloud-sdk/completion.zsh.inc";
      ice = {
        wait = "";
        lucid = true;
      };
    }];
  };
}
