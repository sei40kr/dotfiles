{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.python.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.python.enable (let
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
  in {
    my = {
      home.home.file = foldl (files: name:
        files // {
          ".pyenv/${name}".source = "${pyenv.outPath}/${name}";
        }) { } pyenvRootFiles;

      packages = with pkgs; [
        python37
        poetry
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
      ];

      env = rec {
        PYENV_ROOT = "\${HOME}/.pyenv";
        PATH =
          [ "\${HOME}/.poetry/bin" "${PYENV_ROOT}/bin" "${PYENV_ROOT}/shims" ];
      };
    };
  });
}
