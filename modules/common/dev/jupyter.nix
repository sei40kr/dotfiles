{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.jupyter;
  pythonEnv =
    pkgs.python3.withPackages (p: with p; [ jupyter_core jupyter_client ]);
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

  config = mkIf cfg.enable { user.packages = [ package ]; };
}
