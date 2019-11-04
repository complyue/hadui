/**
 * WebSocket Connection
 *
 */

import { hasLogBox, uiLog, clearLog } from "/log.js";

export class WSC {
  constructor(url) {
    this.url = url;
    this.ws = null;
    this.bins = null;
    this.waiters = null;
  }

  async getUrl() {
    if (this.url) return this.url;
    let wsPort = await $.get("/:");
    return "ws://" + location.hostname + ":" + wsPort;
  }

  init(ws) {
    this.ws = ws;
    this.bins = [];
    this.waiters = [];
  }

  async handleError(err, errDetails) {
    console.error("Unexpected WS error: ", err, errDetails);
    debugger;
    if (hasLogBox()) uiLog(err, "err-msg", errDetails);
  }

  /**
   * return a promise resolving to a connected WebSocket object,
   * which can be used to send outgoing commands asynchronously.
   */
  dial() {
    return new Promise((resolve, reject) => {
      let ws = this.ws;
      let waiters = this.waiters;
      if (null !== ws) {
        if (WebSocket.OPEN === ws.readyState) {
          resolve(ws);
          return;
        }
        if (WebSocket.CONNECTING === ws.readyState) {
          if (null !== waiters) {
            waiters.push([resolve, reject]);
            return;
          } else {
            console.warn("?!");
            debugger;
          }
        }
        this.ws = null;
      }
      if (null !== waiters) {
        this.waiters = null;
        for (let [resolve, reject] of waiters) {
          reject("WS reconnecting");
        }
      }
      this.getUrl().then(
        url => {
          ws = new WebSocket(url);
          ws.binaryType = "arraybuffer";

          this.init(ws);
          let bins = this.bins;
          let waiters = this.waiters;
          waiters.push([resolve, reject]);

          ws.onopen = () => {
            for (let [resolve, reject] of waiters) {
              resolve(ws);
            }
            if (this.waiters === waiters) {
              this.waiters = null;
            }
          };
          ws.onerror = ee => {
            console.error("WS error:", ee);
            // WS error event doesn't contain description, defer handling to close event
            // see: https://stackoverflow.com/a/18804298/6394508
          };
          ws.onclose = ce => {
            console.warn("WS closed.", ce);
            const msg =
              "WebSocket closed, code=" + ce.code + "  reason:" + ce.reason;
            if (1000 != ce.code) {
              this.handleError(msg);
            }
            for (let [resolve, reject] of waiters) {
              reject(new Error(msg));
            }
            if (this.waiters === waiters) {
              this.waiters = null;
            }
          };
          ws.onmessage = me => {
            if (me.data instanceof ArrayBuffer) {
              bins.push(me.data);
              return;
            }
            if ("string" !== typeof me.data) {
              debugger;
              throw "WS msg of type " + typeof me.data + " ?!";
            }

            let json_cmd;
            try {
              json_cmd = JSON.parse(me.data);
            } catch (err) {
              console.log("Error parsing ws msg:", me.data, err);
              let closeWS = true; // can change this to false from debugger console
              debugger;
              if (closeWS) {
                ws.close();
              }
            }
            if ("err" === json_cmd.type) {
              console.error("WS error:", json_cmd);
              let { errText, errDetails } = json_cmd;
              debugger;
              this.handleError(errText, errDetails);
              bins.length = 0; // todo: this really desirable ?
            } else if ("msg" === json_cmd.type) {
              let { msgText, msgType, msgDetails } = json_cmd;
              uiLog(msgText, msgType || "msg", msgDetails);
            } else if ("clear" === json_cmd.type) {
              clearLog();
            } else if ("call" === json_cmd.type) {
              // method call
              let { name, args } = json_cmd;
              try {
                let mth = this[name];
                if (!Array.isArray(args)) {
                  args = [args];
                }
                mth.apply(this, args);
              } catch (err) {
                console.error(
                  "Error calling method " + name + " from WS:",
                  err,
                  json_cmd
                );
                let closeWS = true; // can change this to false from debugger console
                debugger;
                if (closeWS) {
                  ws.close();
                }
              }
            } else {
              console.error("WS msg not understood:", json_cmd);
              let closeWS = true; // can change this to false from debugger console
              debugger;
              if (closeWS) {
                ws.close();
              }
            }
          };
        },
        err => {
          console.error("Error making WS connection:", err);
          debugger;
          this.handleError(err);
          reject(err);
        }
      );
    });
  }

  openWindow(url, windowName) {
    window.open(url, windowName);
  }
}

export default WSC;
