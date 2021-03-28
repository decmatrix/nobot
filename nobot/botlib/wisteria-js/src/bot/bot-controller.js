//TODO: several sessions ?
class BotStateResolver {
    #stateTable;
    #msgStack;
    #nextState;

    constructor() {
        this.#stateTable = {};
    }

    add(name, fn) {
        this.#nextState = name;
        if(this.#stateTable[name] !== undefined) {
            throw new Error(`state ${name} is already exist`);
        }

        this.#stateTable[name] = fn;
    }

    setNextState(name) {
        if(this.#stateTable[name] === undefined) {
            throw new Error(`state ${name} not exist`);
        }

        this.#nextState = name;
    }

    callNext(inputMsg) {
        this.#stateTable[this.#nextState](inputMsg, this.#makeController());
        return this.#msgStack;
    }

    #makeController() {
        this.#msgStack = [];

        return {
            say(msg) {
                this.#msgStack.push(msg);
            },

            next(name) {
                this.setNextState(name);
            }
        }
    }
}

export { BotStateResolver };