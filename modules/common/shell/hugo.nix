{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.shell.hugo;
  package = pkgs.unstable.hugo.overrideAttrs ({ postInstall ? "", ... }: {
    postInstall = ''
      ${postInstall}
      mkdir -p $out/share/{bash-completion/completions,fish/vendor_completions.d,zsh/site-functions}
      $out/bin/hugo gen autocomplete -f $out/share/bash-completion/completions/hugo -t bash
      $out/bin/hugo gen autocomplete -f $out/share/fish/vendor_completions.d/hugo.fish -t fish
      $out/bin/hugo gen autocomplete -f $out/share/zsh/site-functions/_hugo -t zsh
    '';
  });
in {
  options.modules.shell.hugo = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = [ package ]; };
}
