{
  description = "NixOS configuration with blueprint";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs?ref=nixos-unstable";

    # Blueprint framework for automatic module detection
    blueprint.url = "github:numtide/blueprint";
    blueprint.inputs.nixpkgs.follows = "nixpkgs";

    home-manager = {
      url = "github:nix-community/home-manager?ref=release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

    lanzaboote = {
      url = "github:nix-community/lanzaboote/v1.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    llm-agents-nix.url = "github:numtide/llm-agents.nix";

    anthropics-skills = {
      url = "github:anthropics/skills";
      flake = false;
    };

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    idea-LazyVim = {
      url = "github:sei40kr/idea-LazyVim";
      flake = false;
    };

    lazyvim = {
      url = "github:sei40kr/nix-lazyvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    kitty-tmux = {
      url = "github:sei40kr/kitty-tmux";
      flake = false;
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wez-tmux = {
      url = "github:sei40kr/wez-tmux";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wez-pain-control = {
      url = "github:sei40kr/wez-pain-control";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wez-per-project-workspace = {
      url = "github:sei40kr/wez-per-project-workspace";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wez-status-generator = {
      url = "github:sei40kr/wez-status-generator";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      agenix,
      blueprint,
      fenix,
      nixpkgs-unstable,
      ...
    }@inputs:
    (blueprint { inherit inputs; })
    // {
      overlays = import ./overlays;
    };
}
