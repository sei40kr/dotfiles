{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.lang.clojure;
in
{
  options.modules.dev.lang.clojure = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      clojure
    ];

    modules.editors.lspServers.clojure_lsp = rec {
      package = pkgs.clojure-lsp;
      command = "${package}/bin/clojure-lsp";
      filetypes = [
        "clojure"
        "edn"
      ];
      rootMarkers = [
        "project.clj"
        "deps.edn"
        "build.boot"
        "shadow-cljs.edn"
        ".git"
        "bb.edn"
      ];
    };
  };
}
