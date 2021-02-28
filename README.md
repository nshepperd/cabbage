Manages the default ghc package environment, allowing you to install
and remove packages. Almost like a real package manager.

Keeps a registry of installed packages in
`~/.config/cabbage/cabbage.conf`. Invoking `cabbage install` allows
you to manipulate this registry, adding, removing or upgrading
packages, then builds a dummy package with dependencies given by the
new state of the registry and installs the resulting ghc environment
file.

The cabal dependency resolver is used to select the latest possible
package versions when new packages are installed, after which the
selected package versions are remembered until you explicitly upgrade.

The package registry `~/.config/cabbage` is stored in git so that
changes can be easily reverted.

For fine grained control of the build process you can manually edit
`~/.config/cabbage/cabal.project` which will be merged with the dummy
package before building.
