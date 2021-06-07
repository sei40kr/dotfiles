{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.ansible;
  package = pkgs.ansible.overrideAttrs (_: { doCheck = false; });
in {
  options.modules.dev.ansible = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [ package ];

    modules.shell.zsh.zinit.snippets = [{
      source =
        "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/ansible/ansible.plugin.zsh";
      ice.id-as = "OMZP::ansible";
    }];
  };
}
