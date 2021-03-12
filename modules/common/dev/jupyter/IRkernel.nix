{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  ps = pkgs.rPackages;
  rEnv = pkgs.rWrapper.override { packages = with ps; [ IRkernel ]; };
  kernel = {
    displayName = "R";
    argv = [
      "${rEnv}/bin/R"
      "--slave"
      "-e"
      "IRkernel::main()"
      "--args"
      "{connection_file}"
    ];
    language = "R";
    logo32 = null;
    logo64 = "${ps.IRkernel}/library/IRkernel/kernelspec/logo-64x64.png";
  };
in { config = { modules.dev.jupyter.kernels.ir = kernel; }; }
