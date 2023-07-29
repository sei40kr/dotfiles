{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.rust;
in
{
  options.modules.dev.rust = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (fenix.stable.withComponents [
        "cargo"
        "clippy"
        "rust-analyzer"
        "rust-src"
        "rustc"
        "rustfmt"
      ])

      (mkIf config.modules.dev.aws.enable cargo-lambda)
    ];
  };
}
