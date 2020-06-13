{ config, lib, options, pkgs, ... }:

with lib;
(let
  cfg = config.modules.dev.python;
  pyenv = builtins.fetchGit { url = "https://github.com/pyenv/pyenv.git"; };
  pyenvRootFiles = [
    "bin"
    "completions"
    "libexec"
    "plugins/python-build"
    "pyenv.d"
    "src"
    "Makefile"
  ];
  pyenvVirtualenv = builtins.fetchGit {
    url = "https://github.com/pyenv/pyenv-virtualenv.git";
  };
in {
  options.modules.dev.python.enable = mkOption {
    type = types.bool;
    default = false;
  };

  options.modules.dev.python.enablePoetry = mkOption {
    type = types.bool;
    default = true;
  };

  config = mkIf cfg.enable {
    my = {
      home.home.file = (foldl (files: name:
        files // {
          ".pyenv/${name}".source = "${pyenv.outPath}/${name}";
        }) { } pyenvRootFiles) // {
          ".pyenv/plugins/pyenv-virtualenv".source = pyenvVirtualenv.outPath;
        };

      packages = with pkgs;
        ([
          python37
          python37Packages.matplotlib
          python37Packages.numpy
          python37Packages.jupyter
          python37Packages.pandas

          # NOTE pyenv: Python build environment
          #      See https://github.com/pyenv/pyenv/wiki#suggested-build-environment
          bzip2
          libffi
          libxml2
          xmlsec
          openssl
          readline
          sqlite
          lzma
          zlib
        ] ++ optionals cfg.enablePoetry [ poetry ]);

      env = rec {
        PYENV_ROOT = "\${HOME}/.pyenv";
        PATH =
          [ "\${HOME}/.poetry/bin" "${PYENV_ROOT}/bin" "${PYENV_ROOT}/shims" ];
      };
    };

    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice atclone'pyenv init - --no-rehash zsh >pyenv-init.zsh' \
                atpull'%atclone' \
                id-as'pyenv-init'
      zinit light zdharma/null
      zinit ice atclone'pyenv virtualenv-init - --no-rehash zsh >pyenv-virtualenv-init.zsh' \
                atpull'%atclone' \
                id-as'pyenv-virtualenv-init'
      zinit light zdharma/null

      zinit ice as'completion' wait'''
      zinit snippet OMZP::pip/_pip
    '' + optionals cfg.enablePoetry ''
      zinit ice wait''' \
                lucid \
                atclone'poetry completions zsh >_poetry' \
                atpull'%atclone' \
                as'completion' \
                id-as'poetry_completion'
      zinit light zdharma/null
    '';
  };
})
