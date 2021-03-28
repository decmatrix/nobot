//TODO: several sessions ?
class BotStateResolver {
    #stateTable;
    #msgStack;
    #nextState;

    constructor() {
        this.#stateTable = {};
        this.#msgStack = [];
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
        let controller = this.#makeController();
        this.#stateTable[this.#nextState](inputMsg, controller);
        this.setNextState(controller.nextState);
        return controller.msgStack;
    }

    #makeController() {
        this.#msgStack = [];

        return {
            msgStack: [],
            nextState: this.#nextState,

            say(msg) {
                this.msgStack.push(msg);
            },

            next(name) {
                this.nextState = name;
            }
        }
    }
}

export { BotStateResolver };