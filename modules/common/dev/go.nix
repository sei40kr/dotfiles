{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.go;
in {
  options.modules.dev.go = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ go gopls gore ];

    modules.shell.zsh.zinit.snippets = [{
      source =
        "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/golang/golang.plugin.zsh";
      ice.id-as = "OMZP::golang";
    }];
  };
}
