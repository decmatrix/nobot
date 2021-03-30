/*
     Copyright (c) 2021 Bohdan Sokolovskyi
     Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>
 */

import express from 'express';
import session from 'express-session';
import path from 'path';
import { fileURLToPath } from 'url';
import { debug, error } from '../utils/logger.js';
import { Application } from "./application.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const __filename = fileURLToPath(import.meta.url);

class WebApplication extends Application {
    #app;
    #port = 8082;
    #host = 'localhost';
    #staticDir = path.resolve(__dirname + '../../../resources'); //FIXME: hard code
    #baseUrl = '/';
    #asModule = false;

    constructor(options) {
        super();

        this.#port = options.port ?? this.#port;
        this.#host = options.host ?? this.#host;
        this.#baseUrl = options.baseUrl ?? this.#baseUrl;
        this.#staticDir = options.staticDir ?? this.#staticDir;
        this.#asModule = options.asModule ?? this.#asModule;

        this.#app = express();
    }

    getApp() {
        return this.#app;
    }

    configure(bot) {
        this.#app.use(express.json());
        this.#app.use(express.static(this.#staticDir));
        this.#app.use(session({
            saveUninitialized: false,
            resave: false,
            secret: 'secret',
            cookie: {}
        }));

        this.#app.post(
            `${this.#baseUrl}`,
            (req, res) => {
                if(req.body.msg === undefined) {
                    res.status(400);
                    return;
                }

                let resMessages;

                try {
                    //TODO: because express-session is fucking shit
                    if(req.session.cookie.bot === undefined) {
                        req.session.cookie.bot = bot.buildSession();
                    }

                    resMessages = req.session.cookie.bot.stateResolver.callNext(req.body.msg);
                } catch (err) {
                    error(err);
                    res.status(500);
                    return;
                }

                res.send({
                    botName: req.session.cookie.bot.name,
                    messages: resMessages
                });
            }
        );

        return this;
    }

    run() {
        this.#app.listen(this.#port, this.#host, (err) => {
            if(err) {
                error(err);
            }

            debug(`server started on host ${this.#host}, on port ${this.#port}`);
        });
    }
}

export { WebApplication };