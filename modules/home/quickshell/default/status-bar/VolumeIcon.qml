import QtQuick
import Quickshell.Services.Pipewire

NFIcon {
    name: {
        var audio = Pipewire.defaultAudioSink?.audio;
        if (!audio) {
            return "";
        }
        if (audio.muted) {
            return "volume-muted";
        }

        var value = audio.volume;
        if (value === 0) {
            return "volume-off";
        }
        if (value <= 0.5) {
            return "volume-low";
        }
        return "volume-high";
    }
}
