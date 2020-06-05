{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./dev ./shell ];

  config.home.sessionVariables.PATH = [ "\$PATH" ];
}
