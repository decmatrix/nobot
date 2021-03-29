import express from 'express';
import path from 'path';
import { fileURLToPath } from 'url';
import { debug, error } from '../../utils/logger.js';
import { Application } from "../application.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const __filename = fileURLToPath(import.meta.url);

class WebApplication extends Application {
    #app;
    #port = 8082;
    #host = 'localhost';
    #staticDir = path.resolve(__dirname + '../../../../resources'); //FIXME: hard code
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

        this.#app.post(
            `${this.#baseUrl}`,
            (req, res) => {
                if(req.body.msg === undefined) {
                    res.status(400);
                    return;
                }

                let resMessages;

                try {
                    resMessages = bot.getStateResolver().callNext(req.body.msg);
                } catch (err) {
                    error(err);
                    res.status(500);
                    return;
                }

                res.send({
                    botName: bot.getName(),
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