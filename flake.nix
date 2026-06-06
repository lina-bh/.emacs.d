{
  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "emacs-overlay/nixpkgs";
  };

  outputs =
    {
      self,
      emacs-overlay,
      flake-utils,
      nixpkgs,
    }:
    flake-utils.lib.eachDefaultSystemPassThrough (
      system:
      let
        inherit (nixpkgs) lib;
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.${system} = {
          default = self.packages.${system}.emacs;

          # emacs = emacs-overlay.packages.${system}.emacs-git-pgtk.pkgs.withPackages (elpa: with elpa; [ magit ]);
          emacs = emacs-overlay.packages.${system}.emacs-git-nox;

          # cask = self.packages.${system}.emacs.emacs.pkgs.cask;
        };
      }
    );
}
