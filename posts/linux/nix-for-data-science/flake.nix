{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      myvscode = pkgs.vscode-with-extensions.override {
        vscodeExtensions = (with pkgs.vscode-extensions; [
        equinusocio.vsc-material-theme
        equinusocio.vsc-material-theme-icons
        vscodevim.vim
        ms-python.python
        ms-python.vscode-pylance
        ms-toolsai.jupyter
        ms-toolsai.jupyter-renderers
        ]);
      };
    in {
      devShell = pkgs.mkShell {
        name = "python-venv";
        venvDir = "./.venv";
        buildInputs = with pkgs; [
          tmux
          myvscode
          (python3.withPackages(ps: with ps; [
            ipython
            pip
            jupyter
            widgetsnbextension
            jupyter-nbextensions-configurator
            hvplot
            holoviews
            jedi-language-server
            ipywidgets
            mypy
            notebook
            lxml
            beautifulsoup4
            statsmodels
            pandas
            numpy
            geopandas
            matplotlib
            pyproj
            folium
            seaborn
            shapely
            xarray
            rioxarray
            rasterio
          ]))
        ];
        postVenvCreation = ''
          unset SOURCE_DATE_EPOCH
          pip install -r requirements.txt
        '';

        shellHook = ''
            export BROWSER=brave
                # Tells pip to put packages into $PIP_PREFIX instead of the usual locations.
    # See https://pip.pypa.io/en/stable/user_guide/#environment-variables.
            export PIP_PREFIX=$(pwd)/_build/pip_packages
            export PYTHONPATH="$PIP_PREFIX/${pkgs.python3.sitePackages}:$PYTHONPATH"
            export PATH="$PIP_PREFIX/bin:$PATH"
            unset SOURCE_DATE_EPOCH
            #jupyter lab
        '';

        postShellHook = ''
          # allow pip to install wheels
          unset SOURCE_DATE_EPOCH
        '';
      };
    }
  );
}
