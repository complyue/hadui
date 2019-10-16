/**
 * ts.js
 *
 * implementation of Naive dates with ISO8601 format
 */
export class NaiveDate {
  static get iNaT() {
    return -9223372036855;
  }

  static get NaT() {
    return new this(this.iNaT);
  }

  static get iNcT() {
    return 9223372036854;
  }

  static get NcT() {
    return new this(this.iNcT);
  }

  static parse(s) {
    if (!s) {
      return null;
    }
    s = String(s);

    if ("NaT" === s) {
      return this.NaT;
    }
    if ("NcT" === s) {
      return this.NcT;
    }

    return new NaiveDate(
      Date.UTC(
        parseInt(s.substr(0, 4)), // year
        s.length > 5 ? parseInt(s.substr(5, 2)) - 1 : 0, // month
        s.length > 8 ? parseInt(s.substr(8, 2)) : 1, // day
        s.length > 11 ? parseInt(s.substr(11, 2)) : 0, // hour
        s.length > 14 ? parseInt(s.substr(14, 2)) : 0, // min
        s.length > 17 ? parseInt(s.substr(17, 2)) : 0, // second
        s.length > 20 ? parseInt(s.substr(20, 3)) : 0 // millisecond
      )
    );
  }

  static str(ts) {
    return new NaiveDate(ts).str();
  }

  constructor(ts_or_date) {
    if (Object.is(ts_or_date, undefined)) {
      // shift current local time as if we are at UTC zone
      this.ts = Date.now() - new Date().getTimezoneOffset() * 60000;
    } else if (ts_or_date instanceof Date) {
      // shift specified local time as if we are at UTC zone
      this.ts = ts_or_date.getTime() - new Date().getTimezoneOffset() * 60000;
    } else if (Number.isSafeInteger(ts_or_date)) {
      // treat integer as milliseconds since epoch
      this.ts = ts_or_date;
    } else if ("NaT" === ts_or_date) {
      this.ts = this.constructor.iNaT;
    } else if ("NcT" === ts_or_date) {
      this.ts = this.constructor.iNcT;
    } else {
      throw "Invalid timestamp: " + ts_or_date;
    }
  }

  [Symbol.toPrimitive](hint) {
    if ("number" === hint) {
      return this.ts;
    }
    return this.str();
  }

  valueOf() {
    return this.ts;
  }

  toString() {
    return `NaiveDate("${this.str()}")`;
  }

  str(invalid_str) {
    if (this.ts === this.constructor.iNaT) {
      return "string" === typeof invalid_str ? invalid_str : "NaT";
    }
    if (this.ts === this.constructor.iNcT) {
      return "string" === typeof invalid_str ? invalid_str : "NcT";
    }

    const d = new Date(this.ts),
      d2 = this.constructor.d2,
      d3 = this.constructor.d3;
    if (d.getUTCMilliseconds() === 0) {
      if (d.getUTCSeconds() === 0) {
        if (d.getUTCMinutes() === 0) {
          if (d.getUTCHours() === 0) {
            return this.isoDate();
          }
          return (
            d.getUTCFullYear() +
            "-" +
            d2(d.getUTCMonth() + 1) +
            "-" +
            d2(d.getUTCDate()) +
            "T" +
            //
            d2(d.getUTCHours())
            //
          );
        }
        return (
          d.getUTCFullYear() +
          "-" +
          d2(d.getUTCMonth() + 1) +
          "-" +
          d2(d.getUTCDate()) +
          "T" +
          //
          d2(d.getUTCHours()) +
          ":" +
          d2(d.getUTCMinutes())
          //
        );
      }
      return this.isoDateTime(invalid_str);
    }
    return this.isoFull(invalid_str);
  }

  isoDate(invalid_str) {
    if (this.ts === this.constructor.iNaT) {
      return "string" === typeof invalid_str ? invalid_str : "NaT";
    }
    if (this.ts === this.constructor.iNcT) {
      return "string" === typeof invalid_str ? invalid_str : "NcT";
    }

    const d = new Date(this.ts),
      d2 = this.constructor.d2,
      d3 = this.constructor.d3;
    return (
      d.getUTCFullYear() +
      "-" +
      d2(d.getUTCMonth() + 1) +
      "-" +
      d2(d.getUTCDate())
      //
    );
  }

  isoDateTime(invalid_str) {
    if (this.ts === this.constructor.iNaT) {
      return "string" === typeof invalid_str ? invalid_str : "NaT";
    }
    if (this.ts === this.constructor.iNcT) {
      return "string" === typeof invalid_str ? invalid_str : "NcT";
    }

    const d = new Date(this.ts),
      d2 = this.constructor.d2,
      d3 = this.constructor.d3;
    return (
      d.getUTCFullYear() +
      "-" +
      d2(d.getUTCMonth() + 1) +
      "-" +
      d2(d.getUTCDate()) +
      "T" +
      //
      d2(d.getUTCHours()) +
      ":" +
      d2(d.getUTCMinutes()) +
      ":" +
      d2(d.getUTCSeconds())
      //
    );
  }

  isoFull(invalid_str) {
    if (this.ts === this.constructor.iNaT) {
      return "string" === typeof invalid_str ? invalid_str : "NaT";
    }
    if (this.ts === this.constructor.iNcT) {
      return "string" === typeof invalid_str ? invalid_str : "NcT";
    }

    const d = new Date(this.ts),
      d2 = this.constructor.d2,
      d3 = this.constructor.d3;
    return (
      d.getUTCFullYear() +
      "-" +
      d2(d.getUTCMonth() + 1) +
      "-" +
      d2(d.getUTCDate()) +
      "T" +
      //
      d2(d.getUTCHours()) +
      ":" +
      d2(d.getUTCMinutes()) +
      ":" +
      d2(d.getUTCSeconds()) +
      "." +
      //
      d3(d.getUTCMilliseconds())
      //
    );
  }

  idMonth(invalid_str) {
    if (this.ts === this.constructor.iNaT) {
      return "string" === typeof invalid_str ? invalid_str : "NaT";
    }
    if (this.ts === this.constructor.iNcT) {
      return "string" === typeof invalid_str ? invalid_str : "NcT";
    }

    const d = new Date(this.ts),
      d2 = this.constructor.d2,
      d3 = this.constructor.d3;
    return (
      d.getUTCFullYear() + d2(d.getUTCMonth() + 1)
      //
    );
  }

  idDay(invalid_str) {
    if (this.ts === this.constructor.iNaT) {
      return "string" === typeof invalid_str ? invalid_str : "NaT";
    }
    if (this.ts === this.constructor.iNcT) {
      return "string" === typeof invalid_str ? invalid_str : "NcT";
    }

    const d = new Date(this.ts),
      d2 = this.constructor.d2,
      d3 = this.constructor.d3;
    return (
      d.getUTCFullYear() + d2(d.getUTCMonth() + 1) + d2(d.getUTCDate())
      //
    );
  }

  idHMS(invalid_str) {
    if (this.ts === this.constructor.iNaT) {
      return "string" === typeof invalid_str ? invalid_str : "NaT";
    }
    if (this.ts === this.constructor.iNcT) {
      return "string" === typeof invalid_str ? invalid_str : "NcT";
    }

    const d = new Date(this.ts),
      d2 = this.constructor.d2,
      d3 = this.constructor.d3;
    return (
      d2(d.getUTCHours()) + d2(d.getUTCMinutes()) + d2(d.getUTCSeconds())
      //
    );
  }

  idHMSS(invalid_str) {
    if (this.ts === this.constructor.iNaT) {
      return "string" === typeof invalid_str ? invalid_str : "NaT";
    }
    if (this.ts === this.constructor.iNcT) {
      return "string" === typeof invalid_str ? invalid_str : "NcT";
    }

    const d = new Date(this.ts),
      d2 = this.constructor.d2,
      d3 = this.constructor.d3;
    return (
      d2(d.getUTCHours()) +
      d2(d.getUTCMinutes()) +
      d2(d.getUTCSeconds()) +
      d3(d.getUTCMilliseconds())
      //
    );
  }

  static d2(i) {
    if (i < 10) return "0" + i;
    else return "" + i;
  }

  static d3(i) {
    if (i < 10) return "00" + i;
    else if (i < 100) return "0" + i;
    else return "" + i;
  }
}

export default NaiveDate;
