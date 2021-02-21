{ config, lib, ... }:

with lib;
with lib.my; {
  options.modules.dev.editors.ideavim.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.ideavim.enable {
    home.file.".ideavimrc".text = ''
      " Do not exit visual mode on a selection shift
      vnoremap < <gv
      vnoremap > >gv
    '';
  };
}
