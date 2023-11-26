{ config, ... }:

{
  config = {
    user.home = "/Users/${config.user.name}";
  };
}
