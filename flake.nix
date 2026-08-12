{
  description = "A basic flake with a shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/26.05";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        myPackages = {
          pythonEnv = pkgs.python3.withPackages (
            ps: with ps; [
              ipython
              pip
              jupyter
              widgetsnbextension
              great-tables
              googlemaps
              ipympl
              jupyter-nbextensions-configurator
              jedi-language-server
              osmnx
              ipywidgets
              libpysal
              mypy
              hvplot
              pandas
              us
              seaborn
              numpy
              geopandas
              geodatasets
              pyogrio
              geopy
              matplotlib
              pyproj
              osmpythontools
              folium
              mapclassify
              scipy
              networkx
              shapely
              xarray
              rioxarray
              dask
              intake
              intake-parquet
              pooch
              fiona
              plotly
              s3fs
              rasterio
              sympy
              cement
              # jpterm
              # contextily
              # movingpandas
              # geoviews
              # mapclassify
              # stonesoup
            ]
          );
          patchedQuarto = pkgs.quarto.overrideAttrs (oldAttrs: {
            postPatch = (oldAttrs.postPatch or "") + ''
              substituteInPlace bin/quarto.js \
                --replace-fail "syntax-highlighting" "highlight-style"
            '';
          });
        };
      in
      {
        devShells.default = pkgs.mkShell {
          venvDir = "./.venv";
          buildInputs = [
            pkgs.fontconfig
          ];
          packages = builtins.attrValues {
            inherit (myPackages)
              patchedQuarto
              pythonEnv
              ;
            inherit (pkgs)
              R
              # quarto
              chromium
              pandoc
              rstudio
              texliveMedium
              ;
            inherit (pkgs.rPackages)
              pagedown
              tidyverse
              sf
              terra
              leaflet
              # leaflet_extras
              leaflet_extras2
              maps
              leafsync
              elevatr
              trackeR
              zoo
              patchwork
              collapse
              postcards
              XML
              xml2
              gt
              gtExtras
              janitor
              ggpubr
              kit
              microbenchmark
              glue
              rcompanion
              ggridges
              scales
              classInt
              spdep
              nngeo
              ggtext
              zeallot
              gpx
              tidygeocoder
              crsuggest
              stplanr
              rnaturalearth
              rnaturalearthdata
              quarto
              osmdata
              viridis
              usethis
              tmap
              tidycensus
              plotly
              geodata
              prettymapr
              ggspatial
              rstatix
              tidyterra
              htmlwidgets
              basemaps
              trajr
              webshot
              reticulate
              ;
          };
          shellHook = ''
            export PIP_PREFIX=$(pwd)/venvDir
            export PYTHONPATH="$PIP_PREFIX/${pkgs.python3.sitePackages}:$PYTHONPATH"
            export PATH="$PIP_PREFIX/bin:$PATH"
            unset SOURCE_DATE_EPOCH
          '';

        };
      }
    );
}
