import { ExpressServer } from "../server-side/web/express.js"
import { BotStateResolver } from "./bot-controller.js"

const BOT_TYPES = ['chat']
const BOT_PLATFORMS = ['web', 'telegram']

export class Bot {
    #options
    #app
    #stateResolver
    #isConfigured = false

    constructor(options) {
        this.#options = options
        this.#app = new ExpressServer(
            this.#options.getPort(),
            this.#options.getHost()
        )
        this.#stateResolver = new BotStateResolver()
    }

    on(stateName, callback) {
        this.#stateResolver.add(stateName, callback)
    }

    configure() {

        return this;
    }

    runFrom(startStateName) {
        if(!this.#isConfigured) {
            throw new Error("bot is not configured");
        }

        this.#stateResolver.setNextState(startStateName)

    }
}

export class BotOptions {
    #name
    #port
    #host
    #type
    #platform

    constructor(name, port, host) {
        this.#name = name
        this.#port = port
        this.#host = host
    }

    setName(name) {
        this.#name = name
        return this
    }

    setPort(port) {
        this.#port = port
        return this
    }

    setHost(host) {
        this.#host = host
        return this
    }

    setType(type) {
        this.#type = type
        this.#checkType()
        return this
    }

    setPlatform(platform) {
        this.#platform = platform
        this.#checkPlatform()
        return this
    }

    getName() {
        return this.#name
    }

    getPort() {
        return this.#port
    }

    getHost() {
        return this.#host
    }

    getType() {
        return this.#type
    }

    getPlatform() {
        return this.#platform
    }

    toString() {
        return `name: ${this.#name}, port: ${this.#port}, host: ${this.#host}`
    }

    #checkType() {
        if(!BOT_TYPES.find(this.#type)) {
            throw new Error(`Unknown bot type: ${this.#type}`)
        }
    }

    #checkPlatform() {
        if(!BOT_PLATFORMS.find(this.#platform)) {
            throw new Error(`Unknown bot platform: ${this.#platform}`)
        }
    }
}