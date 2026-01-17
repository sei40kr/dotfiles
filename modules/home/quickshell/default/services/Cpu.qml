pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    property list<CpuCore> cores

    Component.onCompleted: {
        updateCpuInfo();
    }

    Timer {
        interval: 2000
        running: true
        repeat: true
        onTriggered: root.updateCpuInfo()
    }

    function updateCpuInfo() {
        cpuProc.running = true;
    }

    Component {
        id: cpuCoreComponent
        CpuCore {}
    }

    function cleanupOldCores() {
        for (var i = 0; i < root.cores.length; i++) {
            root.cores[i].destroy();
        }
    }

    function parseLine(line) {
        var parts = line.split(/\s+/);

        // Skip if line starts with "Average:" - we only want the first measurement
        if (parts[0] === "Average:")
            return null;

        // Time format: "11:52:21 PM 0 ..." (time is 2 parts)
        // Need at least: time(2) + cpu(1) + 10 stats = 13 parts
        if (parts.length < 13)
            return null;

        var cpuId = parts[2];
        if (cpuId === "all")
            return null;

        var coreNum = parseInt(cpuId);
        if (isNaN(coreNum))
            return null;

        return cpuCoreComponent.createObject(root, {
            core: coreNum,
            usr: parseFloat(parts[3]) || 0,
            nice: parseFloat(parts[4]) || 0,
            sys: parseFloat(parts[5]) || 0,
            iowait: parseFloat(parts[6]) || 0,
            irq: parseFloat(parts[7]) || 0,
            soft: parseFloat(parts[8]) || 0,
            steal: parseFloat(parts[9]) || 0,
            guest: parseFloat(parts[10]) || 0,
            gnice: parseFloat(parts[11]) || 0,
            idle: parseFloat(parts[12]) || 0
        });
    }

    Process {
        id: cpuProc
        command: ["mpstat", "-P", "ALL", "1", "1"]
        running: false

        property var pendingCores: []

        stdout: StdioCollector {
            onStreamFinished: {
                if (!this.text)
                    return;

                var lines = this.text.trim().split("\n");
                cpuProc.pendingCores = [];

                for (var i = 0; i < lines.length; i++) {
                    var line = lines[i].trim();
                    if (!line)
                        continue;

                    var coreObj = root.parseLine(line);
                    if (coreObj) {
                        cpuProc.pendingCores.push(coreObj);
                    }
                }

                if (cpuProc.pendingCores.length > 0) {
                    root.cleanupOldCores();
                    root.cores = cpuProc.pendingCores;
                }
            }
        }

        onExited: (exitCode, exitStatus) => {
            if (exitCode !== 0) {
                console.error("Cpu: mpstat process exited with code", exitCode);
            }
        }
    }
}
