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

    modules.shell.zsh.rcInit = ''
      zinit ice wait''' lucid as'completion' id-as'OMZP::rust'
      zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/rust/_rust
    '';
  };
}
