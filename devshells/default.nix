{
  inputs,
  pkgs,
  ...
}:

let
  inherit (inputs.self.checks.${pkgs.stdenv.hostPlatform.system}.pre-commit-check)
    shellHook
    enabledPackages
    ;
in
pkgs.mkShell {
  inherit shellHook;
  buildInputs = enabledPackages;
}
