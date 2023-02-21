# Emurgo CDP

### HW recommendations:

    Ubuntu on VPS, virtualbox or WSL
    At least 30GB harddrive
    Decent RAM

### Installing NIX

    sudo apt update
    sudo apt upgrade -y

Make Single user installation. For details go to https://nixos.org

    sh <(curl -L https://nixos.org/nix/install) --no-daemon

or 

    sh <(curl -L https://nixos.org/nix/install) --daemon
        
 Do not forget to source the path as recommended at the end of the installation: 
 
    . /home/<use_name>/.nix-profile/etc/profile.d/nix.sh
        
Edit nix.conf file to decrease the amount of time for building.
In case of not having /etc/nix/nix.confg create ~/.config/nix/nix.conf instead. Add the following lines:

    build-users-group = nixbld
    allow-import-from-derivation = true
    substituters = https://cache.iog.io https://cache.nixos.org/
    trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
    extra-experimental-features = nix-command flakes
    keep-outputs = true
    keep-derivations = true
      
Confirm that the changes were taken by running

    nix show-config
      
### Installing GHC and Cabal

      curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
      
 Follow the instructions and install in separate command line the dependencies.
 
 Normally the dependencies are:
 
    sudo apt install build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
 
 Check GHC and Cabal version

    ghc --version
    cabal --version

My current ghc and cabal versions are GHC 8.10.7 and Cabal 3.6.2.0. If any version mismatch, you can update with:

    ghcup install ghc 8.10.7
    ghcup install cabal 3.6.2.0

Now set the versions:

    ghcup set ghc 8.10.7
    ghcup set cabal 3.6.2.0

Re-check your current GHC and Cabal version. If it's still not the correct version, try to restart your terminal.

### Building Plutus app

Important to checkout the tag for the week to be compiled; this can be found in the plutus-pioneer-program repo, in the cabal.project file for each week.

Clone the repo plutus core git

    git clone https://github.com/input-output-hk/plutus-apps.git
    cd plutus-apps
    git checkout v2022-04-06

Build Plutus PAB library core with Nix

    nix build -f default.nix plutus-apps.haskell.packages.plutus-pab.components.library

### Start the plutus playground

Start nix-shell

    nix-shell

In plutus-playground-client folder start the server

    cd plutus-playground-client
    plutus-playground-server

In other terminal start the Playground client with nix-shell

    nix-shell

    cd plutus-playground-client 
    npm run start
 
 ### Build plutus documentation
 
 In other terminal in the plutus-apps folder
 
     nix-shell
     build-and-serve-docs

### Access the playground

it should be running on: https://localhost:8009

This completes the installation

##
## Running the project

    git clone https://github.com/larestrepo/emurgocdp.git

> Make sure that the checkout of your plutus-apps is the same as the specified in your cabal.project file. 

If this is the first time, you might want to rerun nix build before to start nix-shell:

    nix build -f default.nix plutus-apps.haskell.packages.plutus-pab.components.library

With nix-shell now activated go to <b>starter</b> folder:

    cabal update (optional)
    cabal build
    cabal repl


