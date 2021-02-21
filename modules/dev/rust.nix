{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.rust.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.rust.enable {
    modules = {
      dev.editors.tools.packages = with pkgs.unstable; [ rust-analyzer ];
      shell.zsh.zinitPluginsInit = ''
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

    user.packages = with pkgs; [ rustup ];
    env = rec {
      CARGO_HOME = "\${HOME}/.cargo";
      PATH = [ "${CARGO_HOME}/bin" ];
    };
  };
}
