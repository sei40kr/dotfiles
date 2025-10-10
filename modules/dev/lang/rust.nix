{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.dev.lang.rust;
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

    modules.editors.lspServers.rust_analyzer = rec {
      package = pkgs.fenix.stable.rust-analyzer;
      command = "${package}/bin/rust-analyzer";
      args = [ ];
      filetypes = [ "rust" ];
      rootMarkers = [
        "Cargo.toml"
        "rust-project.json"
        ".git"
      ];
    };
  };
}
