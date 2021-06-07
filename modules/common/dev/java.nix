{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.java;
in {
  options.modules.dev.java = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ gradle maven ];

    modules.shell.zsh.zinit.snippets = [
      {
        source =
          "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/gradle/gradle.plugin.zsh";
        ice = {
          trigger-load = [ "!gradle" ];
          id-as = "OMZP::gradle";
        };
      }
      {
        source = "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/mvn/mvn.plugin.zsh";
        ice.id-as = "OMZP::mvn";
      }
    ];
  };
}
