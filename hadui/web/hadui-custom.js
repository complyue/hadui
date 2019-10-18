/**
 * hadui-custom.js
 */

import WSC from "/wsc.js";

export const HaduiDefaultStmt =
  // the statement shown initially
  `
-- interactive Haskell statement here --
{- you have the same artifacts as with \`stack ghci\` in scope
you \`import UIO (print)\` somewhere in your project,
or more preferably \`import UIO\`, to get \`print\` -}
print "Hello - web front!"
`.trim();

export class HaduiWSC extends WSC {
  // implement ws methods here
}
