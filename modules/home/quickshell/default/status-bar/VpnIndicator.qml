import QtQuick
import "../services"
import "../theme"

NFIcon {
    name: "vpn"
    color: Theme.successColor
    visible: Array.from(Vpn.connections).some(conn => conn.active)
}
