#!/usr/bin/env bash

if ! hash protonvpn 2>/dev/null; then
  exit
fi

if [[ "$ROFI_RETV" == 1 ]]; then
  args=(${ROFI_INFO})
  sudo protonvpn "${args[@]}" >/dev/null &
  disown
  exit
fi

echo -e '\0no-custom\x1ftrue'
echo -e 'VPN: Disconnect\0info\x1fd'
echo -e 'ProtonVPN: Connect to the fastest server\0info\x1fc -f'
echo -e 'ProtonVPN: Connect to a random server\0info\x1fc -r'
echo -e 'ProtonVPN: Connect to the fastest Secure-Core server\0info\x1fc -sc'
echo -e 'ProtonVPN: Connect to the fastest torrent server\0info\x1fc --p2p'
echo -e 'ProtonVPN: Connect to the fastest Tor server\0info\x1fc --tor'
