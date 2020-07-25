{ config, lib, ... }:

with lib; {
  options.modules.dev.editors.ideavim.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.ideavim.enable {
    my.home.home.file.".ideavimrc".text = ''
      source ${../../../idea-doom-emacs/ideavimrc}

      " Do not exit visual mode on a selection shift
      vnoremap < <gv
      vnoremap > >gv
    '';
  };
}
