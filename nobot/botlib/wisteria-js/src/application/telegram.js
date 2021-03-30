/*
     Copyright (c) 2021 Bohdan Sokolovskyi
     Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>
 */

import { Telegraf } from 'telegraf';
import {debug, error} from '../utils/logger.js';
import { Application } from "./application.js";

class TelegramApplication extends Application {
    #app;
    #token;

    constructor(options) {
        super();

        this.#token = options.token;

        if(this.#token === undefined) {
            throw new Error('undefined token option');
        }

        this.#app = new Telegraf(this.#token);
    }

    configure(bot) {
        this.#app.on(
            'text',
            (ctx, next) => {
                let resMessages;

                try {
                    resMessages = bot.getStateResolver().callNext(ctx.message.text);
                } catch (err) {
                    error(err);
                    return;
                }

                for(let msg of resMessages) {
                    ctx.telegram.sendMessage(ctx.message.chat.id, msg);
                }
            });

        return this;
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