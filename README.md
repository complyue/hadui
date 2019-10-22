# Web front UI for interactive stack projects

## Platform Supports

- macOS - mainly used on Mojave
- Linux - mainly used on Ubuntu 18.04
- Windows - should work in Docker in theory, not attempted yet

## Quick Start

- create a stack project with `hadui` as one of its dependencies,
  in its `package.yaml` like:

```yaml
dependencies:
  - base
  - rio

  - hadui
```

- have `extra-deps` in your project's `stack.yaml` including:

```yaml
extra-deps:
  - github: complyue/hadui
    commit: 5dd260fada86157998df985bfb6df38a430df5f6
    subdirs:
      - hadui

  # need this line until network-3.x goes into lts
  - network-3.1.1.0
```

- besides your `stack.yaml`, create `hadui.yaml`, like:

```yaml
# this cfg file 'hadui.yaml' is located by hadui besides
# the nearest 'stack.yaml' up from cwd you run hadui.
# it uses `stack path --project-root` to locate the dir.

# by default, only the local interface is bound to listen
# for hadui ws connections, this hardens the arbitrary
# code execution vulnerability a bit. but for you or your
# collegues to access hadui web front from other machines,
# bind to one of the external interfaces, or simply all
# interfaces by specifying '0.0.0.0'.
bind-interface: 0.0.0.0
http-port: 5050
ws-port: 5051

# backend log level
log-level: DEBUG

# need a GHC executable supports ':frontend <PluginModule>' command
# see https://gitlab.haskell.org/ghc/ghc/issues/17348#note_228587
with-ghc: ghc-ife

# additional options passed to GHCi
ghci-options:
  # run project code compiled, instead of interpreted,
  # for performance
  - -fobject-code

# additional options passed to both GHC and GHCi
ghc-options:
  # language features to be used
  - -XBlockArguments
  - -XBangPatterns
  - -XLambdaCase

  # limit parallelism on a developer's machine
  - -with-rtsopts=-maxN3
```

- in your stack project, run:

```shell
stack build --exec hadui
```

- or run development mode:

```shell
stack build --exec hadui-dev
```

> in development mode, you just refresh the browser page, the
> backend process will be restarted, changed project source will
> be recompiled automtically.

- optionally, customize front UI for your project, by creating a
  `hadui` folder besides your `stack.yaml`.

  take for example:
  https://github.com/complyue/hadui-demo/tree/master/hadui

## Demo

```shell
git clone https://github.com/complyue/hadui-demo
cd hadui-demo
stack build --exec hadui
```

![hadui-demo-2-fe](https://user-images.githubusercontent.com/15646573/67268165-6410f880-f4e6-11e9-861e-ed779493d6af.png)
![hadui-demo-2-be](https://user-images.githubusercontent.com/15646573/67268163-63786200-f4e6-11e9-86bc-6820a1314477.png)
