{ inputs, perSystem, ... }:
{
  # Minimal home-manager configuration for testing

  # Add home-manager modules incrementally for testing
  # Start with empty imports
  imports = [
    # Modules will be added in each phase
  ];

  home.stateVersion = "23.11";
}
