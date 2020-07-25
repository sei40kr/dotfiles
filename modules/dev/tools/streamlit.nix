{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.streamlit.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.streamlit.enable {
    my.packages = with pkgs; [ streamlit ];
  };
}
