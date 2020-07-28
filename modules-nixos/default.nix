{ lib, ... }:

with lib; {
  imports = [ ./desktop ./dev ./media ./services ./themes ];
}
