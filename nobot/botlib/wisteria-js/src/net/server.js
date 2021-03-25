'use strict'

const express = require('express');

const log = require('../utils/logger');

class Server {
    #app = null;
    #port = 8082;
    #host = "localhost";
    #dirOfStaticResources = `${__dirname}/../../resources`;

    constructor(port=8082, host="localhost") {
        this.#port = 8082;
        this.#host = host;
        this.#app = express();
    }

    setDirOfStaticResources(dir) {
        this.#dirOfStaticResources = dir;
    }

    getApplication() {
        this.#app;
    }

    getPort() {
        return this.#port;
    }

    getHost() {
        return this.#host;
    }

    getDirOfStaticResource() {
        return this.#dirOfStaticResources;
    }

    run() {
        this.#configure();
        
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

    #configure() {
        this.#app.use(express.json());
        this.#app.use(express.static(this.#dirOfStaticResources));
    }
}