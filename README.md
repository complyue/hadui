# Web front UI for interactive stack projects

## Orientation

hadui is data science oriented,
it is not suitable as a general purpose web framework.

all exported functions from all modules in the stack project of matter,
are exposed to frontend in a flat space. this is ideal to support analytical
workflows, but overly open or even prohibitive to support business workflows.

## Platform Support

- macOS - regularly used on Mojave
- Linux - regularly used on Ubuntu 18.04
- Windows - should work in Docker in theory, not attempted yet

### GHC

currently you have to be comfortable to compile yourself an experimental version of
[GHC 8.6.5 with `:frontend` cmd](https://gitlab.haskell.org/complyue/ghc/tree/ghc-8.6-ife)
to start using hadui.

do [this trick](https://gitlab.haskell.org/ghc/ghc/issues/17348#note_228587)
to incorporate it into your stack's GHC installation.

the mod to GHC is very light, should be no difficulty to migrate to other GHC versions,
but as time being, not attempted yet. a MR to GHC is thought of but not carried out yet.

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
    commit: stable
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
> backend process will be restarted, and changed project source
> get recompiled automatically.

- optionally, customize front UI for your project, by creating a
  `hadui` folder besides your `stack.yaml`.

  take for example:
  https://github.com/complyue/hadui-demo/tree/master/hadui

  - the [Rating.hs module](https://github.com/complyue/hadui-demo/blob/master/demo/src/Rating.hs)
    is paired with [rating.html page](https://github.com/complyue/hadui-demo/blob/master/hadui/rating.html) to use state in frontend only.
    ![hadui-rating](https://user-images.githubusercontent.com/15646573/67364542-54ef8080-f5a2-11e9-946f-b4c88cfd8177.png)

  - the [StatefulRating.hs module](https://github.com/complyue/hadui-demo/blob/master/demo/src/StatefulRating.hs)
    is paired with [stateful-rating.html page](https://github.com/complyue/hadui-demo/blob/master/hadui/stateful-rating.html) to use state in backend.
    ![hadui-stateful-rating](https://user-images.githubusercontent.com/15646573/67364543-55881700-f5a2-11e9-9499-10a488e2c818.png)

  - the [updateRank js method](https://github.com/complyue/hadui-demo/blob/master/hadui/hadui-custom.js#L41) is shared by above 2 examples to update UI from Haskell code.

## Demo

```shell
git clone https://github.com/complyue/hadui-demo
cd hadui-demo
stack build --exec hadui
```

![hadui-demo-2-fe](https://user-images.githubusercontent.com/15646573/67268165-6410f880-f4e6-11e9-861e-ed779493d6af.png)
![hadui-demo-2-be](https://user-images.githubusercontent.com/15646573/67268163-63786200-f4e6-11e9-86bc-6820a1314477.png)
