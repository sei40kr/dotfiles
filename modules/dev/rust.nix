{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.rust.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.rust.enable {
    my = {
      packages = with pkgs; [ rustup ];
      env = rec {
        CARGO_HOME = "\${HOME}/.cargo";
        PATH = [ "${CARGO_HOME}/bin" ];
      };
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
