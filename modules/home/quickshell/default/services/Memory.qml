pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    property real totalMemory: 0
    property real usedMemory: 0
    property real availableMemory: 0
    property real usagePercent: 0

    Component.onCompleted: {
        updateMemoryInfo();
    }

    Timer {
        interval: 2000  // Update every 2 seconds
        running: true
        repeat: true
        onTriggered: root.updateMemoryInfo()
    }

    function updateMemoryInfo() {
        memProc.running = true;
    }

    Process {
        id: memProc
        command: ["free", "-b"]
        running: false

        stdout: StdioCollector {
            onStreamFinished: {
                if (!this.text) return;

                var lines = this.text.trim().split("\n");
                if (lines.length < 2) return;

                // Parse the "Mem:" line
                var memLine = lines[1];
                var parts = memLine.split(/\s+/);

                if (parts.length >= 7) {
                    // free -b output: Mem: total used free shared buff/cache available
                    root.totalMemory = parseInt(parts[1]) || 0;
                    root.usedMemory = parseInt(parts[2]) || 0;
                    root.availableMemory = parseInt(parts[6]) || 0;

                    if (root.totalMemory > 0) {
                        root.usagePercent = (root.usedMemory / root.totalMemory) * 100;
                    }
                }
            }
        }

        onExited: (exitCode, exitStatus) => {
            if (exitCode !== 0) {
                console.error("Memory: free command failed with exit code", exitCode);
            }
        }
    }

    function formatBytes(bytes) {
        if (bytes === 0) return "0 B";

        var k = 1024;
        var sizes = ["B", "KB", "MB", "GB", "TB"];
        var i = Math.floor(Math.log(bytes) / Math.log(k));

        return (bytes / Math.pow(k, i)).toFixed(1) + " " + sizes[i];
    }
}
