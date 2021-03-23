'use strict'

const libName = "wisteria";

module.exports = {
    debug: function(...args) {
        args.unshift(`[${libName}] - <DEBUG> /${Date.now()}/:`);
        console.log.apply(console, args);
    },
    warn: function(...args) {
        args.unshift(`[${libName}] - <WARN> /${Date.now()}/:`);
        console.warn.apply(console, args);
    },
    error: function(...args) {
        args.unshift(`[${libName}] - <ERROR> /${Date.now()}/:`);
        console.error.apply(console, args);
    },
}