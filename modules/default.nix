{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./desktop ./dev ./shell ];

  config.home.sessionVariables.PATH = [ "\$PATH" ];
}
