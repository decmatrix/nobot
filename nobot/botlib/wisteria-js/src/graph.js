'use strict'

class Vertex {
    #name = '';
    #action = null;
    #type = 'out';

    constructor(name, action, type) {
        if(typeof action !== 'function') {
            throw Error(`Expected type function, but got ${typeof action}`);
        }

        if(typeof name !== 'string') {
            throw Error(`Expected type string, but got ${typeof name}`);
        }

        if(typeof type !== 'string') {
            throw Error(`Expected type string, but got ${typeof type}`);
        }

        this.#name = name.toLowerCase();
        this.#action = action;
    }

    isIn() {
        return this.#type === 'in';
    }

    isOut() {
        return this.#type === 'out';
    }

    getName() {
        return this.#name;
    }

    getAction() {
        return this.#action;
    }
}