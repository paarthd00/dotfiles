{
  description = "Nix-first setup for rang";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, nixgl }:
    let
      lib = nixpkgs.lib;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = lib.genAttrs systems;
      mkPkgs = system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [ nixgl.overlay ];
        };
      rangModule = import ./nix/modules/rang.nix;
    in
    {
      homeManagerModules.default = rangModule;
      homeManagerModules.rang = rangModule;

      packages = forAllSystems (
        system:
        let
          pkgs = mkPkgs system;
          packageSet = import ./nix/package-set.nix { inherit lib pkgs; };
          bundle = pkgs.buildEnv {
            name = "rang-packages";
            paths = packageSet.full;
          };
        in
        {
          default = bundle;
          rang = bundle;
          rang-packages = bundle;
        }
      );

      homeConfigurations.default =
        let
          system = "x86_64-linux";
          pkgs = mkPkgs system;
          username =
            let value = builtins.getEnv "USER"; in
            if value != "" then value else "p";
          homeDirectory =
            let value = builtins.getEnv "HOME"; in
            if value != "" then value else "/home/${username}";
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            rangModule
            {
              home = {
                inherit username homeDirectory;
                stateVersion = "25.05";
              };

              programs.home-manager.enable = true;

              rang.enable = true;
              rang.theme =
                let value = builtins.getEnv "RANG_THEME"; in
                if value != "" then value else "night-owl";
            }
          ];
        };
    };
}
