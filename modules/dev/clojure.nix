{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.clojure;
in
{
  options.modules.dev.clojure = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ clojure clojure-lsp ];
  };
}
