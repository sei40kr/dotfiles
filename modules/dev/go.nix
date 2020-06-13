{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.go.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.go.enable (let
    goenv = builtins.fetchGit { url = "https://github.com/syndbg/goenv.git"; };
    goenvRootFiles = [ "bin" "completions" "libexec" "plugins" "src" ];
  in {
    my = {
      home.home.file = foldl (files: name:
        files // {
          ".goenv/${name}".source = "${goenv.outPath}/${name}";
        }) { } goenvRootFiles;

      packages = with pkgs; [ go ];
      env = rec {
        GOENV_ROOT = "\${HOME}/.goenv";
        PATH = [ "${GOENV_ROOT}/bin" "${GOENV_ROOT}/shims" ];
      };
    };

    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice atclone'goenv init - --no-rehash zsh >goenv-init.zsh' \
                atpull'%atclone' \
                id-as'goenv-init'
      zinit light zdharma/null

      zinit snippet OMZP::golang/golang.plugin.zsh
    '';
  });
}
