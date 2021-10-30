#!/usr/bin/env bash

if ! hash protonvpn 2>/dev/null; then
  exit
fi

if [[ "$ROFI_RETV" == 1 ]]; then
  case "$ROFI_INFO" in
    disconnect)
      sudo protonvpn d
      exit
      ;;
    connect-sc)
      sudo protonvpn c --sc
      exit
      ;;

    connect-p2p)
      sudo protonvpn c --p2p
      exit
      ;;

    connect-tor)
      sudo protonvpn c --tor
      exit
      ;;
  esac
fi

echo -e '\0no-custom\x1ftrue'
echo -e 'VPN: Disconnect\0info\x1fdisconnect'
echo -e 'VPN: Connect to the fastest Secure-Core server\0info\x1fconnect-sc'
echo -e 'VPN: Connect to the fastest torrent server\0info\x1fconnect-p2p'
echo -e 'VPN: Connect to the fastest Tor server\0info\x1fconnect-tor'
