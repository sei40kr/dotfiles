{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.term.sensible;
in
{
  options.modules.term.sensible = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.term.wezterm.enable
        || config.modules.term.kitty.enable;
      message = "The sensible module requires at least one terminal emulator installed.";
    }];

    user.packages = with pkgs; [ my.sensible-terminal ];
  };
}
