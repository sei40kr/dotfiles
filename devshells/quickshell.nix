{
  pkgs,
  ...
}:

let
  quickshell-dev = pkgs.writeShellScriptBin "quickshell-dev" ''
    if [ ! -d "$PWD/modules/home/quickshell/default" ]; then
      echo "Error: modules/home/quickshell/default directory not found in $PWD"
      echo "Please run this command from the dotfiles root directory"
      exit 1
    fi

    # Send sample notifications in background after quickshell starts
    (
      sleep 0.5
      notify-send -u low 'Low Priority' 'This is a low urgency notification'
      notify-send 'Normal Notification' 'Body text that spans multiple lines to test word wrapping behavior in the notification popup'
      notify-send -u critical 'Critical Alert' 'This is a critical notification'
      notify-send 'Summary Only'
      notify-send 'Duplicate Test' 'Same content'
      notify-send 'Duplicate Test' 'Same content'
      notify-send 'This is an extremely long summary that should be truncated with ellipsis at the end' 'Short body'
      notify-send 'Long Body Test' 'This notification has a very long body text that should wrap across multiple lines and eventually get truncated with an ellipsis when it exceeds the maximum number of allowed lines in the notification popup display area'
    ) &

    exec quickshell -p "$PWD/modules/home/quickshell/default" "$@"
  '';
in
pkgs.mkShell {
  buildInputs = [
    pkgs.libnotify
    quickshell-dev
  ];

  shellHook = ''
    echo "QuickShell development shell"
    echo "============================"
    echo ""
    echo "Commands:"
    echo "  quickshell-dev  Launch QuickShell with local config and sample notifications"
    echo "  notify-send     Send individual notifications"
  '';
}
