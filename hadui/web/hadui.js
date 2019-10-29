/**
 * hadui.js
 */

import { uiLog } from "/log.js";

import { HaduiWSC } from "./hadui-custom.js";

export const haduiWSC = new HaduiWSC();

export async function withHadui(haduiAct) {
  let ws = await haduiWSC.dial();
  haduiAct(ws);
}

$(async function() {
  try {
    uiLog("Dialing Hadui backend ...");
    await haduiWSC.dial();
  } catch (err) {
    let details = err ? err.stack : err;
    uiLog("Failed connecting to Hadui backend via ws.", "err-msg", details);
  }
});

export default haduiWSC;
