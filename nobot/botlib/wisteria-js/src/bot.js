/*
     Copyright (c) 2021 Bohdan Sokolovskyi
     Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>
 */

import _ from 'lodash';

const BOT_TYPES = ['chat'];

class Bot {
    #name;
    #type;
    #startFrom;
    #sessionData = {};
    #stateContainer;

    static chatType() {
        return 'chat';
    }

    constructor(options) {
        this.#name = options.name;
        this.#type = options.type;
        this.#startFrom = options.startFrom;
        this.#stateContainer = new StateContainer();

        if(this.#name === undefined) {
            throw new Error('undefined \'name\' option');
        }

        if(this.#type === undefined) {
            throw new Error('undefined \'type\' option')
        }

        if(options.startFrom === undefined) {
            throw new Error('undefined \'startFrom\' option');
        }

        if(!BOT_TYPES.includes(this.#type)) {
            throw new Error(`unknown type: ${this.#type}`);
        }
    }

    // register session data for each user
    use(sessionData) {
        this.#sessionData = sessionData;
        return this;
    }

    buildSession() {
        return {
            name: this.#name,
            stateResolver: new StateResolver(
                this.#stateContainer,
                this.#startFrom,
                _.cloneDeep(this.#sessionData) ?? this.#sessionData
            )
        };
    }

    getName() {
        return this.#name;
    }

    on(stateName, callback) {
        this.#stateContainer.add(stateName, callback);
    }
}

class StateContainer {
    #stateTable = {};
    #lastAdded;

    constructor() {}

    add(name, fn, redefine) {
        if(!redefine && this.#stateTable[name] !== undefined) {
            throw new Error(`state ${name} is already exist`);
        }

        this.#lastAdded = name;
        this.#stateTable[name] = fn;

        return this;
    }

    remove(name) {
        this.#stateTable[name] = undefined;
        return this;
    }

    get(name) {
        let resolvedState = this.#stateTable[name];
        if(resolvedState === undefined) {
            throw new Error(`stat ${name} not exist`);
        }

        return resolvedState;
    }

    getLastAdded() {
        return name;
    }
}

class StateResolver {
    #stateContainer;
    #nextState;
    #sessionData;

    constructor(stateContainer, startState, sessionData) {
        if(!(stateContainer instanceof StateContainer)) {
            throw new Error('expected state container');
        }

        this.#stateContainer = stateContainer;
        this.#sessionData = sessionData;
        this.#nextState = startState ?? this.#stateContainer.getLastAdded();
    }

    //TODO: here maybe not only text message
    callNext(msg) {
        let controller = new StateController(this.#sessionData);
        this.#stateContainer.get(this.#nextState)(msg, controller);
        this.#nextState = controller.getNextState();
        console.log(this.#nextState);
        //TODO: here maybe not only text messages
        return controller.getInternalData();
    }
}

class StateController {
    #internalData = [];
    #externalData = {};
    #nextState;

    constructor(externalData) {
        this.#externalData = externalData ?? this.#externalData;
    }

    // API methods
    save(what, to) {
        if(this.#externalData[to] === undefined) {
            throw new Error(`undefined external data: ${to}`);
        }

        this.#externalData[to] = what;
        return this;
    }

    //TODO: maybe not only data
    say(msg) {
        this.#internalData.push(msg);
        return this;
    }

    next(state) {
        this.#nextState = state;
        return this;
    }
    ////////

    getInternalData() {
        return this.#internalData;
    }

    getExternalData() {
        return this.#externalData;
    }

    getNextState() {
        return this.#nextState;
    }

    resetInternalData() {
        this.#internalData = [];
        return this;
    }
}

export { Bot };