{ lib, ... }:

with lib; {
  imports = [ ./desktop ./dev ./media ./themes ];
}
