{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.python.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.python.enable (let
    pyenv = builtins.fetchGit { url = "https://github.com/pyenv/pyenv.git"; };
  in {
    home = {
      packages = with pkgs; [
        python37
        poetry
        python37Packages.matplotlib
        python37Packages.numpy
        python37Packages.jupyter
        python37Packages.pandas
      ];

      sessionVariables = rec {
        PYENV_ROOT = pyenv.outPath;
        PATH =
          [ "\${HOME}/.poetry/bin" "${PYENV_ROOT}/bin" "${PYENV_ROOT}/shims" ];
      };
    };
  });
}
