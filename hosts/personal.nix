{ pkgs, ... }:

{
  user = {
    isNormalUser = true;
    name = "sei40kr";
    uid = 1000;
    extraGroups = [ "wheel" ];
    shell = pkgs.zsh;
  };
}
