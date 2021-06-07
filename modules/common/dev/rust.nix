{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.rust;
in {
  options.modules.dev.rust = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ rustup rust-analyzer ];
    env = rec {
      CARGO_HOME = "\${HOME}/.cargo";
      PATH = [ "${CARGO_HOME}/bin" ];
    };

    modules.shell.zsh.zinit.snippets = [{
      source = "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/rust/_rust";
      ice = {
        wait = "";
        lucid = true;
        as = "completion";
        id-as = "OMZP::rust";
      };
    }];
  };
}
