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
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.emacs = self.packages.${system}.emacs-with-packages;

        packages.emacs-with-packages = emacs-overlay.lib.${system}.emacsWithPackagesFromUsePackage {
          config = ./init.el;
          package = pkgs.emacs30-pgtk;
          extraEmacsPackages =
            epkgs: with epkgs; [
              # embark-consult
              # caddyfile-mode
              # markdown-mode
              # nix-mode
              # org
              # gcmh
              # auctex
              (treesit-grammars.with-grammars (
                gram: with gram; [
                  tree-sitter-bash
                  tree-sitter-dockerfile
                  tree-sitter-java
                  tree-sitter-json
                  tree-sitter-tsx
                  tree-sitter-typescript
                ]
              ))
            ];
        };
      }
    );
}
