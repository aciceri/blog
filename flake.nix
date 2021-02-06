{
  description = "My blog";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    flake-utils.url = github:numtide/flake-utils;
    nur.url = github:nix-community/NUR;
    katex-src = {
      url = "https://github.com/KaTeX/KaTeX/releases/download/v0.12.0/katex.tar.gz";
      flake = false;
    };
    typoproweb-src = {
      url = "https://github.com/rse/typopro-web/archive/4.2.5.tar.gz";
      flake = false;
    };
    firacode-src = {
      url = github:tonsky/FiraCode;
      flake = false;
    };
    hyphenopoly-src = {
      url = "https://github.com/mnater/Hyphenopoly/archive/v4.10.0.tar.gz";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, nur, katex-src, typoproweb-src, firacode-src, hyphenopoly-src }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ nur.overlay ]; };
        blog = pkgs.callPackage ./blog.nix {
          inherit pkgs;
          thirdparty = [
            {
              name = "katex";
              path = "${katex-src}";
            }
            {
              name = "baskerville";
              path = "${typoproweb-src}/web/TypoPRO-LibreBaskerville";
            }
            {
              name = "firacode";
              path = "${firacode-src}/distr/woff2";
            }
            {
              name = "hyphenopoly";
              path = "${hyphenopoly-src}";
            }
          ];
        };
      in
        rec {
          packages = { inherit (blog) generator ci shell; };
          defaultPackage = blog.generator;
          apps.compile =
            flake-utils.lib.mkApp { drv = blog.ci.compile; exePath = ""; };
          apps.generator =
            flake-utils.lib.mkApp { drv = blog.generator-with-thirdparty; exePath = "/bin/generator"; };

          defaultApp = apps.compile;
          devShell = blog.shell;
        }
    );
}
  
