{ config, lib, ... }:

with lib;
with lib.my; {
  config = {
    user.home = "/Users/${config.user.name}";
  };
}
