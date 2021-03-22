'use strict'

const Graph = require('graphlib').Graph;

class Bot {
    #options = null;
    #vertexes = [];
    #entryPoint = null;

    constructor(options) {
        if(!(options instanceof BotOptions)) {
            throw Error(`Expected type BotOptions, but got ${typeof options}`);
        }

        this.#options = options;
    }

    addVertex(vertex) {
        if(!(vertex instanceof Vertex)) {
            throw Error(`Expected type Vertex, but got ${typeof vertex}`);
        }

        this.#vertexes.push(vertex);
    }

    setEntryPoint(vertex) {
        if(!(vertex instanceof Vertex)) {
            throw Error(`Expected type Vertext, but got ${typeof vertext}`);
        }
    }

    build() {
        
        return this;
    }

    run() {
        
    }

}

class BotOptions {
    #name = '';
    #port = 8082;
    #host = 'localhost';

    constructor(name, port=8081, host='localhost') {
        this.#name = name;
        this.#port = port;
        this.#host = host;
    }

    setPort(port) {
        this.#port = port;
    }

    setHost(host) {
        this.#host = host;
    }

    getName() {
        return this.#name;
    }

    getPost() {
        return this.#port;
    }

    getHost() {
        return this.#host;
    }

    toString() {
        return `name: ${this.#name}, port: ${this.#port}, host: ${this.#host}`;
    }
}

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
