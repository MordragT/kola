{
  description = "Rust development template";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fenix = {
      url = "github:nix-community/fenix/monthly";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    systems.url = "github:nix-systems/default";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      imports = [
        inputs.git-hooks.flakeModule
        ./nix
      ];

      perSystem =
        { pkgs, ... }:
        {
          pre-commit = {
            settings = {
              package = pkgs.prek;
              hooks = {
                nixfmt = {
                  enable = true;
                  id = "nixfmt";
                  after = [ "statix" ];
                };
                statix = {
                  enable = true;
                  id = "statix";
                };
              };
            };
          };
        };
    };
}
