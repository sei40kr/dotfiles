{ lib, ... }:

with lib; {
  imports = [ ./desktop ./term ];
}
