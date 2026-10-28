{ pkgs }:

{
  pomodoro = import ./pomodoro.nix { inherit pkgs; };
}
