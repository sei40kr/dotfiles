{ alacritty, lib, ... }:

with lib;
let icon = ./icons/alacritty-big-sur-icon.icns;
in alacritty.overrideAttrs ({ postPatch ? "", ... }: {
  postPatch = ''
    ${postPatch}
    cp -f ${icon} extra/osx/Alacritty.app/Contents/Resources/alacritty.icns
  '';
})
