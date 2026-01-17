import QtQuick

QtObject {
    required property string name
    required property string type  // "openvpn" or "wireguard"
    required property bool active
}
