{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  pythonEnv = pkgs.python3.withPackages
    (ps: (with ps; [ ipykernel numpy matplotlib pandas ]));
  pythonWrapper = pkgs.writeShellScriptBin "python" ''
    export PYTHONPATH=${pythonEnv}/${pythonEnv.sitePackages}
    ${pythonEnv.interpreter} "$@"
  '';
  kernel = {
    displayName = "Python 3";
    argv = [
      "${pythonWrapper}/bin/python"
      "-m"
      "ipykernel_launcher"
      "-f"
      "{connection_file}"
    ];
    language = "python";
    logo32 =
      "${pythonEnv}/${pythonEnv.sitePackages}/ipykernel/resources/logo-32x32.png";
    logo64 =
      "${pythonEnv}/${pythonEnv.sitePackages}/ipykernel/resources/logo-64x64.png";
  };
in { config = { modules.dev.jupyter.kernels.python3 = kernel; }; }
