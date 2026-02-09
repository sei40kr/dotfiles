{
  config,
  inputs,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.rust;
in
{
  options.modules.dev.lang.rust = {
    enable = mkEnableOption "Rust development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      (perSystem.fenix.stable.withComponents [
        "cargo"
        "clippy"
        "rust-analyzer"
        "rust-src"
        "rustc"
        "rustfmt"
      ])
      pkg-config
      openssl
      # TODO: Add cargo-lambda when aws module is enabled
    ];

    modules.ai.permissions.allowedCommandPrefixes = [
      "cargo build"
      "cargo test"
      "cargo check"
      "cargo clippy"
      "cargo fmt"
      "cargo doc"
      "cargo tree"
      "cargo metadata"
    ];
    modules.ai.permissions.allowedFetchDomains = [
      "docs.rs"
      "crates.io"
    ];

    modules.editors.lspServers.rust_analyzer = {
      package = null;
      command = "rust-analyzer";
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
