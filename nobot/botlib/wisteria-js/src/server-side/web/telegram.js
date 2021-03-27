import { Telegraf } from "telegraf"
import { debug } from '../../utils/logger.js'

class TelegramServer {
    #app

    constructor(telegramOptions) {
        this.#app = new Telegraf(telegramOptions.getToken())
    }

    configure() {
        
    }

    run() {
        debug('server for telegram bot started')
        this.#app.startPolling()
    }
}

export { TelegramServer }