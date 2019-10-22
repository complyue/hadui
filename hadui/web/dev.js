/**
 * dev.js
 */

import { uiLog, clearLog } from "/log.js";

import { withHadui } from "/hadui.js";

import { HaduiDefaultStmt } from "./hadui-custom.js";

let mainCodeDiv = document.getElementById("main_code");
export const mainEditor = mainCodeDiv
  ? CodeMirror(mainCodeDiv, {
      value: HaduiDefaultStmt,
      lineNumbers: true,
      mode: "haskell",
      keyMap: "sublime",
      autoCloseBrackets: true,
      matchBrackets: true,
      showCursorWhenSelecting: true,
      theme: "monokai",
      tabSize: 2,
      viewportMargin: Infinity
    })
  : null;

$("button[name=crunch]").on("click", () =>
  withHadui(ws => {
    let mainCode = mainEditor.getValue();
    uiLog("Executing stmt:", "msg", mainCode);
    ws.send(mainEditor.getValue());
  })
);
$("button[name=clear-log]").on("click", () => {
  clearLog();
});
