import QtQuick
import "../services"

NFIcon {
    name: "vpn"
    color: Theme.vpnConnected
    visible: Array.from(Vpn.connections).some(conn => conn.active)
}
