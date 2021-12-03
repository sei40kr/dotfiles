{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.wayland;
in { options.modules.desktop.wayland = { enable = mkBoolOpt false; }; }
