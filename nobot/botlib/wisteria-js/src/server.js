'use strict'

const express = require('express');

const log = require('./logger');

class Server {
    #app = null;
    #port = 8082;
    #host = "localhost";

    constructor(port=8082, host="localhost") {
        this.#port = 8082;
        this.#host = host;
        this.#app = express();
    }
}

// getters
Server.prototype.getApplication = function() {
    return this.#app;
}

Server.prototype.getPort = function() {
    return this.#port;
}

Server.prototype.getHost = function() {
    return this.#host;
}


Server.prototype.run = function() {
    this.#app.listen(
        this.#port, 
        this.#host,
        (err) => {
            if(err) {
                log.error(err);
            }

            log.debug(`server started on host ${this.#host}, on port ${this.#port}`);
        }
    );
}