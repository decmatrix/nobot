import { Telegraf } from 'telegraf';
import { debug } from '../../utils/logger.js';
import { Application } from "../application.js";

class TelegramApplication extends Application {
    #app;
    #token;

    constructor(options) {
        super();

        this.#token = options.token ?? throw new Error('undefined token option');

        this.#app = new Telegraf(token);
    }

    configure(bot) {

    }

    getApp() {
        return this.#app;
    }

    run() {
        debug('server for telegram bot started');
        this.#app.startPolling();
    }
}

export { TelegramApplication };