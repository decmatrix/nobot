const libName = "wisteria";

export function debug(...args) {
    args.unshift(`[${libName}] - <DEBUG> /${Date.now()}/:`)
    console.log.apply(console, args)
}

export function warn(...args) {
    args.unshift(`[${libName}] - <WARN> /${Date.now()}/:`)
    console.warn.apply(console, args)
}

export function error(...args) {
    args.unshift(`[${libName}] - <ERROR> /${Date.now()}/:`)
    console.error.apply(console, args)
}