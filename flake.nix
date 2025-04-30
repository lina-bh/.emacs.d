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
        packages.default = self.packages.${system}.emacs;

        packages.emacs = nixpkgs.legacyPackages.${system}.emacs30-pgtk.pkgs.withPackages (
          epkgs: with epkgs; [
            aggressive-indent
            auctex
            caddyfile-mode
            cape
            consult
            corfu
            delight
            eglot
            embark
            embark-consult
            envrc
            gcmh
            hcl-mode
            magit
            marginalia
            markdown-mode
            modus-themes
            nix-mode
            orderless
            org
            puni
            rust-mode
            vterm
            yaml-mode
            (treesit-grammars.with-grammars (
              gram: with gram; [
                tree-sitter-bash
                tree-sitter-dockerfile
                tree-sitter-java
                tree-sitter-json
                tree-sitter-tsx
                tree-sitter-typescript
                tree-sitter-go
                tree-sitter-rust
              ]
            ))
          ]
        );
      }
    );
}
