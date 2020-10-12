{ rpc-authentication-required, rpc-password, rpc-port, rpc-username }:

{
  profiles = [{
    profile-name = "Default";
    hostname = "localhost";
    port = rpc-port;
    rpc-url-path = "/transmission/rpc";
    username = if rpc-authentication-required then rpc-username else "";
    password = if rpc-authentication-required then rpc-password else "";
    auto-connect = true;
    ssl = false;
    ssl-validate = false;
    timeout = 40;
    retries = 3;
    update-active-only = false;
    activeonly-fullsync-enabled = false;
    activeonly-fullsync-every = 2;
    update-interval = 3;
    min-update-interval = 3;
    session-update-interval = 60;
    exec-commands = [ ];
    destinations = [ ];
  }];
  profile-id = 0;
}
