{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.aws-cli;
  package = pkgs.awscli;
in {
  options.modules.dev.aws-cli = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [ package ];

    modules.shell.zsh.zinit.snippets = [{
      source = "${package}/share/zsh/site-functions/aws_zsh_completer.sh";
      ice = {
        wait = "";
        lucid = true;
      };
    }];
  };
}
