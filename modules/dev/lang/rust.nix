{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.lang.rust;
in
{
  options.modules.dev.lang.rust = {
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
      pkg-config
      openssl
      (mkIf config.modules.dev.tools.aws.enable cargo-lambda)
    ];
  };
}
