/*
     Copyright (c) 2021 Bohdan Sokolovskyi
     Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>
 */

const libName = "wisteria";

export function debug(...args) {
    args.unshift(`[${libName}] - <DEBUG> /${new Date()}/:`);
    console.log.apply(console, args);
}

export function warn(...args) {
    args.unshift(`[${libName}] - <WARN> /${new Date()}/:`);
    console.warn.apply(console, args);
}

export function error(...args) {
    args.unshift(`[${libName}] - <ERROR> /${new Date()}/:`);
    console.error.apply(console, args);
}