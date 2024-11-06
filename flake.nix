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
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages.emacs-with-packages = emacs-overlay.lib.${system}.emacsWithPackagesFromUsePackage {
          config = ./init.el;
          package = emacs-overlay.packages.${system}.emacs-git;
          extraEmacsPackages =
            epkgs: with epkgs; [
              embark-consult
              caddyfile-mode
              markdown-mode
              nix-mode
              org
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
        packages.tools-for-emacs = pkgs.buildEnv {
          name = "tools-for-emacs";
          paths = with pkgs; [
            fd
            biome
            nix-direnv
            nixfmt-rfc-style
            ruff
            yamlfmt
          ];
        };
      }
    );
}
