{ lib, ... }:

with lib; {
  imports = [ ./desktop ./services ./term ];
}
