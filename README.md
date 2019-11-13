# Web front UI for interactive Haskell stack projects

[![Join the chat at https://gitter.im/hadui-web-front/community](https://badges.gitter.im/hadui-web-front/community.svg)](https://gitter.im/hadui-web-front/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## The Idea

The idea behind Hadui is rather simple, just to use a web browser in
place of the traditional terminal based console UI.

And for simplicity, it's achieved by having the web page (i.e. the
frontend) keep life-long
[WebSocket](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API)
connection with the process run in background (i.e. the backend), and use
text packets to:

- in order to control the representation state in browser, json command is
  sent from the backend Haskell program to web page for execution.

- in responding to user interactions with the web page, Haskell code
  is sent from browser to backend process for execution.

This is fundamentally not different than tradition IO approach to implement
Terminal/Text UI for a
[(emulated)](https://en.wikipedia.org/wiki/Terminal_emulator)
[terminal](https://en.wikipedia.org/wiki/Computer_terminal)
, only more flexible as it's inherently easier to extend the
**json commands** (inplace of
[ANSI Escape sequences](http://ascii-table.com/ansi-escape-sequences.php)
) and **Haskell statements** (inplace of
[ASCII control characters](https://en.wiktionary.org/wiki/Appendix:Control_characters)
) compared to terminal based interactions via stdio, and not limited to one
[tty](https://en.wikipedia.org/wiki/Text_terminal)
per process - you can have many web pages open to interact with a single
process.

## Demo

```shell
git clone https://github.com/complyue/hadui-demo
cd hadui-demo
stack build --exec hadui
```

![hadui-demo-fe](https://user-images.githubusercontent.com/15646573/67366682-88340e80-f5a6-11e9-94b7-ed6c66cf428e.png)
![hadui-demo-be](https://user-images.githubusercontent.com/15646573/67366681-88340e80-f5a6-11e9-99ea-a74e6ec54144.png)

## Orientation

Hadui is data science oriented, it is not suitable as a general purpose
web framework.

All exported functions from all modules in the stack project of matter,
are exposed to frontend in a flat name space. This is ideal to support
analytical workflows, but overly open or even prohibitive to support
business workflows.

## Support (or lack thereof)

Commercial support has not been planned, community is not formed yet,
but Hadui is an important part of my internal tool chain at work, it is:

- macOS - regularly used on Mojave
- Linux - regularly used on Ubuntu 18.04
- Windows - should work in Docker in theory, not attempted yet

### Nix

[Cabal's view on developing multiple packages](https://www.haskell.org/cabal/users-guide/nix-local-build.html?highlight=project#developing-multiple-packages)
[Stack's view on package versus project](https://docs.haskellstack.org/en/stable/stack_yaml_vs_cabal_package_file/#package-versus-project)

### GHC and Stack

Hadui is geared to run upon the latest
[LTS Haskell](https://www.stackage.org/lts)
supported by Stackage, but currently an experimental version of
[GHC 8.6.5](https://gitlab.haskell.org/complyue/ghc/tree/ghc-8.6-ife)
is necessarily used as the compiler, while it can be automatically
installed by `stack` for macOS and Linux on x64 hardware. You are
encouraged to compile from source if on other platforms or the
automatically installed bindist does not work for you.

The mod to GHC is very light - simply added `:frontend` cmd to allow a
[Frontend plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#frontend-plugins) be used with GHCi mode,
(pending issue at https://gitlab.haskell.org/ghc/ghc/issues/17348) it
should be no difficulty to migrate to other GHC versions. so far an MR to
GHC is thought of but not carried out yet. Until the official stock
release of GHC has it merged, I would be maintaining custom branches
matching the GHC version chosen by latest
[LTS Haskell](https://www.stackage.org/lts) which is `8.6.5` by
[lts 14.11](https://www.stackage.org/lts-14.11)
at time of speaking.

## (My) Typical Usage

> I believe Hadui can be useful in more ways for you and others, e.g.
> a version of [CodeWorld](https://code.world) capable to leverage
> the computing power of a computing center on the cloud.
>
> (I had been faithful to use CodeWorld as frontend for my team, only
> to find out it runs the program by the browser, i.e. all in frontend,
> no backend. So comes Hadui)

Data analysts use a browser to submit scripts (in native Haskell, for
parameters, simple job control etc.) to trigger number crunching in
the backend (a single Haskell process or a swarm of computing nodes),
and to see results plotted back to the browser - more windows opened
to show [Bokeh](https://bokeh.org) figures (with
[Haze](https://github.com/complyue/haze) as a Hadui
[overlay package](https://github.com/complyue/hadui/wiki/OverlayPackage)
yet under construction).

![hadui-vscode-int-fe](https://user-images.githubusercontent.com/15646573/67581869-558f3f00-f77b-11e9-9e8a-c875a212c80b.png)

Programmers have `hadui-dev` as the default build tool run an ever going
build task in their [VSCode](https://code.visualstudio.com) environment
with [HIE](https://github.com/haskell/haskell-ide-engine) enabled via
[VsCode extension for Haskell](https://github.com/alanz/vscode-hie-server)
, to develop crunching code in stack projects.

![hadui-vscode-int-be](https://user-images.githubusercontent.com/15646573/67583167-ab64e680-f77d-11e9-8574-4d71fd290a25.png)

## The UIO monad/module from package Hadui

Haskell code from Hadui UI runs in the `UIO` monad. It's pretty much the
same as `RIO` from the [rio](https://github.com/commercialhaskell/rio)
library, with few addons like `print`/`uiLog`, and concrete `env` of type
`UserInterfaceOutput`. It is essentially `ReaderT UserInterfaceOutput IO`,
so you can do (un)lifting within it however you need.

```haskell
-- | The monad for User Interface Output
-- UIO is output only, conversely to IO (which stands for Input/Output),
-- user inputs shall be facilitated with a registry of 'MVar's,
-- those get filled with 'IoC' from UI widgets.
newtype UIO a = UIO { unUIO :: ReaderT UserInterfaceOutput IO a }
    deriving (Functor, Applicative, Monad, MonadIO,
        MonadReader UserInterfaceOutput, MonadThrow)
```

After all your code in the stack project has no necessarity to do with `UIO` at all,
[print :: Display a => a -> UIO ()](https://github.com/complyue/hadui/blob/stable/hadui/src/UIO.hs#L34)
can give you a handful hand to show virtually any value to the log box in UI.
(You know the
[Display](https://www.stackage.org/haddock/lts/rio/RIO.html#t:Display)
typeclass, don't you?) And you can always do
[liftIO](https://www.stackage.org/haddock/lts/base/Control-Monad-IO-Class.html#v:liftIO)
or similar to obtain a value within a `do` block as necessary.

And the `UIO` module re-exports
[RIO](https://www.stackage.org/haddock/lts/rio/RIO.html)
, so you can use it for
[Prelude replacement](https://github.com/commercialhaskell/rio#prelude-replacement)
as well as `RIO`.

## Quick Start

- create a stack project with `hadui` as one of its dependencies,
  in its `package.yaml` like:

```yaml
dependencies:
  - base
  - rio

  - hadui
```

- in the project's `stack.yaml`, tell location of Hadui within `extra-deps`,
  and customize the compiler definition:

```yaml
extra-deps:
  # to use the version of Hadui checked out locally
  #- ../hadui/hadui

  # to use a version of Hadui from github
  - github: complyue/hadui
    commit: stable
    subdirs:
      - hadui

compiler-check: match-exact

# CAVEATS
#
#   binary distributions referenced following have NO support, checkout:
#
# https://gitlab.haskell.org/complyue/ghc-ife-bindist/blob/master/README.md

ghc-variant: ife

setup-info:
  ghc:
    macosx-custom-ife:
      8.6.5:
        url: "https://gitlab.haskell.org/complyue/ghc-ife-bindist/raw/master/ghc-8.6.5-x86_64-apple-darwin.tar.gz2"
    linux64-custom-ife:
      8.6.5:
        url: "https://gitlab.haskell.org/complyue/ghc-ife-bindist/raw/master/ghc-8.6.5-x86_64-unknown-linux.tar.xz"
```

- besides your `stack.yaml`, create `hadui.yaml`, like:

```yaml
# this cfg file 'hadui.yaml' is located by Hadui besides
# the nearest 'stack.yaml' up from cwd you run Hadui.
# it uses `stack path --project-root` to locate the dir.

# by default, only the local interface is bound to listen
# for Hadui ws connections, this hardens the arbitrary
# code execution vulnerability a bit. but for you or your
# collegues to access Hadui web front from other machines,
# bind to one of the external interfaces, or simply all
# interfaces by specifying '0.0.0.0'.
bind-interface: 0.0.0.0
http-port: 5050
ws-port: 5051

# backend log level
log-level: DEBUG

# additional options passed to GHCi
ghci-options:
  # max history in trace, GHCi defaults to 50
  - -fghci-hist-size=300

  # run project code compiled, instead of interpreted, for performance
  # but you won't get source locations for uncaught errors at runtime
  #- -fobject-code

# additional options passed to both GHC and GHCi
ghc-options:
  # language extensions to use, recommended by rio
  #     https://github.com/commercialhaskell/rio/#language-extensions
  - -XAutoDeriveTypeable
  # ...
  - -XViewPatterns
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

- and you are good to go

open http://localhost:5050

![hadui-hello](https://user-images.githubusercontent.com/15646573/67655747-a5912000-f98c-11e9-955e-4a4289080aed.png)

![hadui-hello-be](https://user-images.githubusercontent.com/15646573/67656467-d83c1800-f98e-11e9-97cb-f7dd82de48bf.png)

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

- wanna your own front page ?

just create `hadui/front.html`

> all resources under https://github.com/complyue/hadui/tree/stable/hadui/web
> can be preceded by placing a same named file under your project's `hadui`
> folder. there be `front.html` of just symlink to `dev.html`, with your own
> front page inplace, you can still access the good old dev page at:
> http://localhost:5050/dev.html

## VSCode Integration

Setup your [VSCode](https://code.visualstudio.com) environment
with [HIE](https://github.com/haskell/haskell-ide-engine) enabled via
[VsCode extension for Haskell](https://github.com/alanz/vscode-hie-server)

Create under the project root `.vscode/tasks.json` with following contents:

```json
{
  "version": "2.0.0",
  "presentation": {
    "reveal": "always",
    "panel": "new"
  },
  "tasks": [
    {
      "group": {
        "kind": "build",
        "isDefault": true
      },
      "label": "hadui-dev",
      "type": "shell",
      "command": "cd ${workspaceRoot}; stack build --exec hadui-dev"
    }
  ]
}
```

Press `F7` (macOS) or `Ctrl+Shift+B` (Linux) to start `hadui-dev` for the project

![hadui-vscode](https://user-images.githubusercontent.com/15646573/67378020-26c96b00-f5b9-11e9-9780-302db88ff50d.png)

## Debugging with VSCode

coming sooner than later ...
