/**
 * hadui.js
 */

import { uiLog, clearLog } from "/log.js";

import { HaduiWSC } from "./hadui-custom.js";

export const haduiWSC = new HaduiWSC();

export const mainEditor = CodeMirror(document.getElementById("main_code"), {
  value: `-- interactive Haskell statement here --
print "Hello - web front!"
`,
  lineNumbers: true,
  mode: "haskell",
  keyMap: "sublime",
  autoCloseBrackets: true,
  matchBrackets: true,
  showCursorWhenSelecting: true,
  theme: "monokai",
  tabSize: 2,
  viewportMargin: Infinity
});

$("button[name=crunch]").on("click", async function() {
  let ws = await haduiWSC.dial();
  let mainCode = mainEditor.getValue();
  uiLog("Executing stmt:", "msg", mainCode);
  ws.send(mainEditor.getValue());
});
$("button[name=clear-log]").on("click", function() {
  clearLog();
});

$(async function() {
  try {
    uiLog("Preparing hadui backend ...");
    await haduiWSC.dial();
  } catch (err) {
    let details = err ? err.stack : err;
    uiLog("Failed connecting to hadui backend via ws.", "err-msg", details);
  }
});

export default haduiWSC;
