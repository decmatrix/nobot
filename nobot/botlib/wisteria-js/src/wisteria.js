'use strict'

const bot = require("./bot-tools/bot-options");

class WisteriaBot {
    #options = null;

    constructor(options) {
        this.#options = options;
    }
}

WisteriaBot.prototype.getOptions = function() {
    return this.#options;
}

