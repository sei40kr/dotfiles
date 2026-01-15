pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    property list<VpnConnection> connections

    Component.onCompleted: {
        updateVpnStatus();
    }

    Timer {
        interval: 2000
        running: true
        repeat: true
        onTriggered: root.updateVpnStatus()
    }

    function updateVpnStatus() {
        vpnProc.running = true;
    }

    Component {
        id: vpnConnectionComponent
        VpnConnection {}
    }

    function cleanupOldConnections() {
        for (var i = 0; i < root.connections.length; i++) {
            root.connections[i].destroy();
        }
    }

    Process {
        id: vpnProc
        command: ["systemctl", "-q", "--plain", "list-units", "--all", "--type=service", "openvpn-*", "wg-quick-*"]
        running: false

        property var pendingConnections: []

        stdout: StdioCollector {
            onStreamFinished: {
                if (!this.text) {
                    root.cleanupOldConnections();
                    root.connections = [];
                    return;
                }

                var lines = this.text.trim().split("\n");
                vpnProc.pendingConnections = [];

                for (var i = 0; i < lines.length; i++) {
                    var line = lines[i].trim();

                    // Parse systemctl output: "service.name loaded active running description"
                    var parts = line.split(/\s+/);
                    if (parts.length < 4)
                        continue;

                    var serviceName = parts[0];
                    var activeState = parts[2];

                    // Extract service type and name
                    var type = "";
                    var name = "";

                    if (serviceName.startsWith("openvpn-")) {
                        type = "openvpn";
                        name = serviceName.replace("openvpn-", "").replace(".service", "");
                    } else if (serviceName.startsWith("wg-quick-")) {
                        type = "wireguard";
                        name = serviceName.replace("wg-quick-", "").replace(".service", "");
                    } else {
                        continue;
                    }

                    vpnProc.pendingConnections.push(
                        vpnConnectionComponent.createObject(root, {
                            name: name,
                            type: type,
                            active: activeState === "active"
                        })
                    );
                }

                root.cleanupOldConnections();
                root.connections = vpnProc.pendingConnections;
            }
        }

        onExited: (exitCode, exitStatus) => {
            if (exitCode !== 0) {
                console.error("Vpn: systemctl command failed with exit code", exitCode);
            }
        }
    }
}
