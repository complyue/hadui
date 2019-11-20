# Web front UI for interactive Haskell projects

[![Join the chat at https://gitter.im/hadui-web-front/community](https://badges.gitter.im/hadui-web-front/community.svg)](https://gitter.im/hadui-web-front/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Why Hadui

> I had been faithful to use CodeWorld as the workbench for my team, only
> to find out it runs the program by the browser, i.e. all in frontend,
> no backend. So comes Hadui - web UI to Haskell programs in backend.

The idea behind Hadui is rather simple, just to use a web browser in
place of the traditional terminal based console UI. After that, web
technologies (HTML5/WebGL) based GUI & Visualization become native
for Haskell programs.

And for simplicity, it's achieved by having the web page (i.e. the
frontend) keep life-long
[WebSocket](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API)
connection with the process run in background (i.e. the backend), and use
text packets for commu:

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

And a plus: binary packets/streams through the WebSocket can be used to
communicate binary data efficiently between frontend and backend.

## Requirements

With a rather small codebase, and the most extraordinary
dependencies being
[websockets](http://hackage.haskell.org/package/websockets)
and
[snap-server](http://hackage.haskell.org/package/snap-server)
, Hadui is no more than a vanilla Haskell program (with
[rio](http://hackage.haskell.org/package/rio)
as the replacement Prelude if you care), actually you can hack it
all the way you'd like.

But for productivity, in composing vast Haskell pieces into
an interactive context, with fast development iterations in mind, we
choose to dynamicly compile & execute a Haskell project underlying.
Therefore this
[pending feature request for interactive frontend support in GHCi](https://gitlab.haskell.org/ghc/ghc/issues/17348)
, it's not in stock GHC yet, now lives in
[this experimental GHC branch](https://gitlab.haskell.org/complyue/ghc/tree/ghc-8.6-ife)
as time being. Meaning you need a custom built GHC to use
Hadui for now.

Fortunately [Nix](https://nixos.org/nix) can save our asses in building
GHC from source painlessly, at the mere cost of **several GBs** of disk
space and one-shot build time of about **one hour** each GHC version.

So besides the source available from its github repository, Hadui
is only distributed via [Nixpkgs](https://nixos.org/nixpkgs/).

Mind you that Nix is Linux focused, with decent support for macOS but
Windows is not well supported. Much the same Hadui is.

## Quick Start

[Install Nix](https://github.com/complyue/hadui/wiki/InstallNix) if not
already.

There're 3 flavors of Hadui project you can choose freely to start with,
each with a scaffold template ready. To setup an interactive Haskell
project with web UI:

### [Stack](https://haskellstack.org) based

```shell
curl -L https://github.com/complyue/hadui-demo-stack/archive/master.tar.gz | tar xzf -
mv hadui-demo-stack-master my-awsome-project
cd my-awsome-project
nix-shell --run hadui
```

### [Cabal](https://www.haskell.org/cabal) based

```shell
curl -L https://github.com/complyue/hadui-demo-cabal/archive/master.tar.gz | tar xzf -
mv hadui-demo-cabal-master my-awsome-project
cd my-awsome-project
nix-shell --run hadui
```

### Barebone GHCi

```shell
curl -L https://github.com/complyue/hadui-demo-nix/archive/master.tar.gz | tar xzf -
mv hadui-demo-nix-master my-awsome-project
cd my-awsome-project
nix-shell --run hadui
```

## Orientation

Hadui is data science oriented, it is not suitable as a general purpose
web framework.

Artifacts from the underlying project are exposed to frontend in a flat
name space. This is ideal to support analytical workflows, but overly
open or even prohibitive to support business workflows.

### GHC versions

The mod to add
[interactive frontend support](https://gitlab.haskell.org/ghc/ghc/issues/17348)
to GHC is very light - simply added `:frontend` cmd to allow a
[Frontend plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#frontend-plugins) be used with GHCi.

I would be maintaining custom branches matching the GHC version chosen
by latest
[LTS Haskell](https://www.stackage.org/lts) as well as
[Nixpkgs](https://nixos.org/nixpkgs/), which is `8.6.5`
at time of speaking.

## (My) Typical Usage

> I believe Hadui can be useful in more ways for you and others, e.g.
> a version of [CodeWorld](https://code.world) capable to leverage
> the computing power of a computing center on the cloud.

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
, to develop crunching code in Haskell projects.

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

After all, your code in the Haskell project has no necessarity to do with `UIO` at all,
[print :: Display a => a -> UIO ()](https://github.com/complyue/hadui/blob/master/hadui/src/UIO.hs#L34)
can give you a handful hand to show virtually any value to the log box in UI.
(check out the
[Display](https://www.stackage.org/haddock/lts/rio/RIO.html#t:Display)
typeclass) And you can always do
[liftIO](https://www.stackage.org/haddock/lts/base/Control-Monad-IO-Class.html#v:liftIO)
or similar to obtain a value within a `do` block as necessary.

And the `UIO` module re-exports
[RIO](https://www.stackage.org/haddock/lts/rio/RIO.html)
, so you can use it for
[Prelude replacement](https://github.com/commercialhaskell/rio#prelude-replacement)
as well as `RIO`.

## Demo

After started the project you've created from any of the scaffolds as
instructed above, open http://localhost:5050

> Tip: there's development mode (run `hadui-dev` instead of `hadui`), where
> you can just refresh the browser page after source modification in the project,
> the backend process will be restarted, and changed source will be recompiled
> automatically.

![hadui-hello](https://user-images.githubusercontent.com/15646573/67655747-a5912000-f98c-11e9-955e-4a4289080aed.png)

![hadui-hello-be](https://user-images.githubusercontent.com/15646573/67656467-d83c1800-f98e-11e9-97cb-f7dd82de48bf.png)

- you customize front UI for your project, by having a
  `hadui` folder besides `hadui.yaml` under the project root.

  take for example:
  https://github.com/complyue/hadui-demo-stack/tree/master/hadui

  - the [Rating.hs module](https://github.com/complyue/hadui-demo-stack/blob/master/hadui-demo/src/Rating.hs)
    is paired with [rating.html page](https://github.com/complyue/hadui-demo-stack/blob/master/hadui/rating.html) to use state in frontend only.
    ![hadui-rating](https://user-images.githubusercontent.com/15646573/67364542-54ef8080-f5a2-11e9-946f-b4c88cfd8177.png)

  - the [StatefulRating.hs module](https://github.com/complyue/hadui-demo-stack/blob/master/hadui-demo/src/StatefulRating.hs)
    is paired with [stateful-rating.html page](https://github.com/complyue/hadui-demo-stack/blob/master/hadui/stateful-rating.html) to use state in backend.
    ![hadui-stateful-rating](https://user-images.githubusercontent.com/15646573/67364543-55881700-f5a2-11e9-9499-10a488e2c818.png)

  - the [updateRank js method](https://github.com/complyue/hadui-demo-stack/blob/master/hadui/hadui-custom.js#L41) is shared by above 2 examples to update UI from Haskell code.

- wanna your own front page ?

just create `hadui/front.html` under your project root.

> all resources under https://github.com/complyue/hadui/tree/master/hadui/web
> can be preceded by placing a same named file under your project's `hadui`
> folder. there be `front.html` of just symlink to `dev.html`, with your own
> front page inplace, you can still access the good old dev page at:
> http://localhost:5050/dev.html

## VSCode Integration

Setup your [VSCode](https://code.visualstudio.com) environment
with [HIE](https://github.com/haskell/haskell-ide-engine) enabled via
[VsCode extension for Haskell](https://github.com/alanz/vscode-hie-server)

Just open with VSCode the project you've created from one of the scaffold
templates above, there's already `.vscode/tasks.json` from the scaffold with
following contents:

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
      "command": "cd ${workspaceRoot}; nix-shell --run hadui-dev"
    }
  ]
}
```

Press `F7` (macOS) or `Ctrl+Shift+B` (Linux) to start `hadui-dev` for the project

![hadui-vscode](https://user-images.githubusercontent.com/15646573/67378020-26c96b00-f5b9-11e9-9780-302db88ff50d.png)

## Debugging with VSCode

coming sooner than later ...
