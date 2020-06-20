{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./kaguya ./loz-botw ];
}
