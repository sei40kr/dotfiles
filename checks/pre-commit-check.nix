{ inputs, pkgs, ... }:

let
  treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs ../treefmt.nix;
in
inputs.git-hooks.lib.${pkgs.stdenv.hostPlatform.system}.run {
  src = ./..;
  # Use the Rust-based `prek` to avoid building the upstream `pre-commit`
  # Python package, whose nativeBuildInputs pull in dotnet-sdk and crash
  # `dotnet new nugetconfig` (Abort trap: 6) on aarch64-darwin macOS 26.x.
  package = pkgs.prek;
  hooks = {
    nil = {
      enable = true;
      settings.denyWarnings = true;
    };
    statix.enable = true;
    treefmt = {
      enable = true;
      package = treefmtEval.config.build.wrapper;
    };
  };
}
