pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    property var mountPoints: []

    Component.onCompleted: {
        updateDiskInfo();
    }

    Timer {
        interval: 10000  // Update every 10 seconds (disk usage changes slowly)
        running: true
        repeat: true
        onTriggered: root.updateDiskInfo()
    }

    function updateDiskInfo() {
        diskProc.running = true;
    }

    Process {
        id: diskProc
        command: ["df", "-l", "-B1", "--output=target,size,used,avail,pcent"]
        running: false

        stdout: StdioCollector {
            onStreamFinished: {
                if (!this.text) return;

                var lines = this.text.split("\n");
                var mounts = [];

                // Skip header line, start from line 1
                for (var i = 1; i < lines.length; i++) {
                    var line = lines[i].trim();
                    if (!line) continue;

                    var parts = line.split(/\s+/);

                    // df --output format: Mounted Size Used Avail Use%
                    if (parts.length >= 5) {
                        var mountPoint = parts[0];

                        // Skip non-standard mount points (tmpfs, devtmpfs, etc) and /boot
                        if (mountPoint.indexOf("/dev") === 0 ||
                            mountPoint.indexOf("/sys") === 0 ||
                            mountPoint.indexOf("/proc") === 0 ||
                            mountPoint.indexOf("/run") === 0 ||
                            mountPoint.indexOf("/tmp") === 0 ||
                            mountPoint.indexOf("/boot") === 0) {
                            continue;
                        }

                        var total = parseInt(parts[1]) || 0;
                        var used = parseInt(parts[2]) || 0;
                        var available = parseInt(parts[3]) || 0;
                        var percentStr = parts[4].replace("%", "");
                        var percent = parseFloat(percentStr);

                        if (!isNaN(percent) && total > 0) {
                            mounts.push({
                                mountPoint: mountPoint,
                                totalSpace: total,
                                usedSpace: used,
                                availableSpace: available,
                                usagePercent: percent
                            });
                        }
                    }
                }

                root.mountPoints = mounts;
            }
        }

        onExited: (exitCode, exitStatus) => {
            if (exitCode !== 0) {
                console.error("Disk: df command failed with exit code", exitCode);
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
