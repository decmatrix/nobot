import { BotStateResolver } from "./bot-controller.js";

const BOT_TYPES = ['chat'];

class Bot {
    #name;
    #type;
    #stateResolver;

    constructor(options) {
        this.#name = options.name ?? throw new Error('undefined name option');
        this.#type = options.type;

        if(!BOT_TYPES.find(this.#type)) {
            throw new Error(`unknown type: ${this.#type}`);
        }

        this.#stateResolver = new BotStateResolver();
        this.#stateResolver.setNextState(options.startFrom ?? throw new Error('undefined startFrom option'));
    }

    getName() {
        return this.#name;
    }

    getStateResolver() {
        return this.#stateResolver;
    }

    on(stateName, callback) {
        this.#stateResolver.add(stateName, callback);
    }

    runFrom(startStateName) {
        this.#stateResolver.setNextState(startStateName);
    }
}

export { Bot };