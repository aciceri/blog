{ pkgs, thirdparty }:
  with pkgs;
  let

    script = { ... } @ args: pkgs.nur.repos.ysndr.lib.wrap ({ shell = true; } // args);

    haskellPackagesFixed = haskellPackages.extend (
      self: super: with pkgs.haskell.lib;  {
        hakyll = appendPatch (dontCheck (super.callHackage "hakyll" "4.13.4.1" {})) ./generator/hakyll.patch;
        hakyll-images = unmarkBroken super.hakyll-images;
        hakyll-sass = unmarkBroken super.hakyll-sass;
      }
    );

    thirdpartyFarm = linkFarm "thirdparty" thirdparty;
    
    generator = (haskellPackagesFixed.callCabal2nix "Site" (./generator) {}).overrideAttrs (
      old: {
        nativeBuildInputs = old.nativeBuildInputs or [] ++ [ makeWrapper ];
        installPhase = old.installPhase + "\n" + ''
          wrapProgram $out/bin/generator
        '';
      }
    );

    generator-with-thirdparty = generator.overrideAttrs (old: {
      nativeBuildInputs = old.nativeBuildInputs or [] ++ [ makeWrapper ];
      installPhase = old.installPhase + "\n" + ''
        wrapProgram $out/bin/generator --set THIRDPARTY ${thirdpartyFarm}
      '';
    });

    generate-website = script {
      name = "generate-website";
      paths = [ generator-with-thirdparty git ];

      script = ''
        generator rebuild
      '';
    };

    haskell-env = haskellPackagesFixed.ghcWithHoogle (hp:
      with hp;
      [ haskell-language-server cabal-install ] ++ generator.buildInputs
    );

    shell = mkShell {
      name = "blog-env";
      buildInputs = [
        haskell-env
      ];

      shellHook = ''
        export THIRDPARTY="${thirdpartyFarm}"
        export HAKYLL_ENV="development"
        export HIE_HOOGLE_DATABASE="${haskell-env}/share/doc/hoogle/default.hoo"
        export NIX_GHC="${haskell-env}/bin/ghc"
        export NIX_GHCPKG="${haskell-env}/bin/ghc-pkg"
        export NIX_GHC_DOCDIR="${haskell-env}/share/doc/ghc/html"
        export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
      '';

    };

  in {

    inherit shell generator generator-with-thirdparty generate-website;

    ci = {
      compile = generate-website;
    };

  }

