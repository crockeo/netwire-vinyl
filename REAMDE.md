# netwire-vinyl

This project is a sort of proof-of-concept to the fact that Netwire and
vinyl-gl can come together in a reasonably effective way to create a graphical
and interactive application.

# Installation

Simply clone the repository from one of the following:

```bash
git@github.com:crockeo/netwire-vinyl.git
# Or
https://github.com/crockeo/netwire-vinyl.git
```

After you have everything downloaded on your machine, you're going to want to
set up a `cabal sandbox`, install dependencies, and build the project:

```bash
>$ cabal sandbox init
>$ cabal install --only-dependencies
>$ cabal build
```

This should leave an executable somewhere in the `dist/` folder. I trust you
enough to let you find it (especially since the `cabal build` command will tell
you exactly where it is).
