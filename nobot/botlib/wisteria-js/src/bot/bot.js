import { BotStateResolver } from "./bot-controller.js";

const BOT_TYPES = ['chat'];

class Bot {
    #name;
    #type;
    #stateResolver;
    #startFrom;

    constructor(options) {
        this.#name = options.name;
        this.#type = options.type;

        if(this.#name === undefined) {
            throw new Error('undefined name option');
        }

        if(!BOT_TYPES.includes(this.#type)) {
            throw new Error(`unknown type: ${this.#type}`);
        }

        this.#stateResolver = new BotStateResolver();

        if(options.startFrom === undefined) {
            throw new Error('undefined startFrom option');
        }
        this.#startFrom = options.startFrom;
    }

    configure() {
        this.#stateResolver.setNextState(this.#startFrom);
        return this;
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
}

export { Bot };