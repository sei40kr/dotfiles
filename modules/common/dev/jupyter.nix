{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.dev.jupyter;
  pythonEnv = pkgs.python3.withPackages
    (ps: with ps; [ jupyter_client jupyter_console jupyter_core ]);
  jupyterPath = pkgs.jupyter-kernel.create { definitions = cfg.kernels; };
  package = pkgs.writeShellScriptBin "jupyter" ''
    export PYTHONPATH=${pythonEnv}/${pythonEnv.sitePackages}
    export JUPYTER_PATH=${jupyterPath}
    ${pythonEnv}/bin/jupyter "$@"
  '';
in {
  options.modules.dev.jupyter = with types; {
    enable = mkBoolOpt false;
    kernels = mkOption {
      type = attrs;
      default = { };
      visible = false;
    };
  };

  config = mkIf cfg.enable {
    user.packages = [ package ];
    home.file.".jupyter/jupyter_console_config.py".source =
      "${configDir}/jupyter/jupyter_console_config.py";
  };
}
