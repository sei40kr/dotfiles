{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.circleci-cli;
in {
  options.modules.dev.circleci-cli = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ circleci-cli ]; };
}
