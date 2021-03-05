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
    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice wait''' \
                lucid \
                atclone'rustup completions zsh >_rustup' \
                atpull'%atclone' \
                as'completion' \
                id-as'rustup_completion'
      zinit light zdharma/null
      zinit ice as'completion' wait'''
      zinit snippet OMZP::rust/_rust
      zinit ice wait''' \
                lucid \
                atclone'rustup completions zsh cargo >_cargo' \
                atpull'%atclone' \
                as'completion' \
                id-as'cargo_completion'
      zinit light zdharma/null
    '';
  };
}
