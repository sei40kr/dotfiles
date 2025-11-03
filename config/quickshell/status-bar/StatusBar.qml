import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Pipewire

PanelWindow {
    id: bar
    required property var niri

    anchors {
        top: true
        left: true
        right: true
    }
    implicitHeight: 40

    component Left: RowLayout {
        property int margin: 64
        anchors {
            left: parent.left
            leftMargin: margin
            verticalCenter: parent.verticalCenter
        }
        spacing: 16
    }

    component Center: RowLayout {
        anchors {
            horizontalCenter: parent.horizontalCenter
            verticalCenter: parent.verticalCenter
        }
        spacing: 16
    }

    component Right: RowLayout {
        property int margin: 64
        anchors {
            right: parent.right
            rightMargin: margin
            verticalCenter: parent.verticalCenter
        }
        spacing: 16
    }

    Rectangle {
        anchors.fill: parent
        color: Theme.backgroundColor

        PwObjectTracker {
            objects: [Pipewire.defaultAudioSink]
        }

        Left {
            Workspaces {
                workspaces: niri.workspaces
            }
        }

        Center {
            WindowTitle {
                niri: bar.niri
            }
        }

        Right {
            SystemClock {
                id: clock
                precision: SystemClock.Seconds
            }

            VolumeIcon {}

            Clock {
                systemClock: clock
            }
        }
    }
}
