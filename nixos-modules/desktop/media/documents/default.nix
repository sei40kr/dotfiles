{ lib, ... }:

with lib;
with lib.my;
{
  options.modules.desktop.media.documents = {
    enable = mkBoolOpt false;

    ebook.enable = mkBoolOpt false;
  };
}
