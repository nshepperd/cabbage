# A Package "Manager" for Haskell

Manages the default ghc package environment
(`~/.ghc/x86_64-linux-$version/environments/default`), allowing you to
install and remove packages. Almost like a real package manager.

This is accomplished by keeps a registry of installed packages in
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

Not guaranteed to do anything sensible on Windows.

## Getting Started

Build the executable, and put `cabbage` somewhere in your PATH. Make
sure your cabal package cache is reasonably up to date (`cabal
v2-update`). Install something:

    cabbage install lens

Then invoke ghc/ghci outside of a project and experience the joy of
having libraries available in your repl:

	GHCi, version 8.10.4: https://www.haskell.org/ghc/  :? for help
	Loaded package environment from /home/username/.ghc/x86_64-linux-8.10.4/environments/default
	Loaded GHCi configuration from /home/username/.ghci
	λ> import Control.Lens
	λ>

## Bonus features (not controllable with the command line UI currently)

### `cabal.project`

For fine grained control of the build process you can manually edit
`~/.config/cabbage/cabal.project` which will be appended to the dummy
package's `cabal.project` before building. For instance, you can insert

    allow-newer: hakyll:cryptonite
                 , hakyll:optparse-applicative
                 , hakyll:pandoc
                 , hakyll:random

to work around the `hakyll` package not having its upper bounds updated yet.

### Local packages

You can manually edit `~/.config/cabbage/cabbage.conf`, which is
currently a simple line-based configuration format. Among other
things, this allows you to add local packages by adding a line like:

    local /path/to/gloss-1.13.2.1

Then the local package will be used instead of whatever is on
Hackage. This is useful for patching packages locally, or building
your own projects that you haven't uploaded to Hackage yet.
