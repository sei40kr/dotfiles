# tui.bash
# author: Seong Yong-ju

print-list-item() {
    local title="$1"

    tui-print "- ${title}"
}

print-step() {
    local step="$1"

    tui-print "→ ${step}"
}

ask-yesno() {
    local prompt="$1"

    tui-print-prompt "${prompt} [yn]"

    while true; do
        read -n1 -r answer

        echo
        if [[ "$answer" == [yY]* ]]; then
            return 0
        elif [[ "$answer" == [nN]* ]]; then
            return 1
        else
            tui-print-prompt "Invalid answer. ${prompt} [yn]"
        fi
    done
}
