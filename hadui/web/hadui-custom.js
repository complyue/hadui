/**
 * hadui-custom.js
 */

import WSC from "/wsc.js";

export const HaduiDefaultStmt =
  // the statement shown initially
  `
{- put and execute interactive Haskell statement here.
you have the same artifacts in scope as \`stack ghci\`.
to get \`print\` work here, you re-export \`UIO (print)\`
from some module in your project. -}
print "Hello, web front!"
`.trim();

export class HaduiWSC extends WSC {
  // implement ws methods here
}
