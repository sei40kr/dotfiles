{ inputs, pkgs, ... }:

let
  inherit (inputs.self.checks.${pkgs.stdenv.hostPlatform.system}.pre-commit-check) config;
  inherit (config) package configFile;
  script = ''
    ${pkgs.lib.getExe package} run --all-files --config ${configFile}
  '';
in
pkgs.writeShellScriptBin "pre-commit-run" script
