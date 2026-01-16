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
  imports = [
    inputs.self.homeModules.editor-shared
  ];

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

    modules.editors.lspServers.rust_analyzer = rec {
      package = perSystem.fenix.stable.rust-analyzer;
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
