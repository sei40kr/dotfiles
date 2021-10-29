#!/usr/bin/env bash

protonvpn status | awk '/^Status:/ { status = tolower($2) }
  /^Server:/ { server = $2 }
  END { printf "{\"text\":\"%s\",\"alt\":\"%s\",\"class\":\"%s\"}", server, status, status }'
