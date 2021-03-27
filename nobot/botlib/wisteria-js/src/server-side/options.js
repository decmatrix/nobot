class WebOptions {
    #host
    #port
    #baseUrl
    #asRouterModule = false

    constructor(host, port, baseUrl) {
        this.#host = host
        this.#port = port
        this.#baseUrl = baseUrl
    }

    setHost(host) {
        this.#host = host ?? process.env.HOST ?? 'localhost'
        return this
    }

    setPort(port) {
        this.#port = port ?? process.env.PORT ?? 3000
        return this
    }

    setBaseUrl(baseUrl) {
        this.#baseUrl = baseUrl ?? process.env.BASE_URL ?? '/'
        return this
    }

    setAsRouterModule(asRouterModule) {
        this.#asRouterModule = asRouterModule ?? false
        return this
    }

    getHost() {
        if(this.#host === undefined) {
            throw new Error('host undefined')
        }

        return this.#host
    }

    getPost() {
        if(this.#port === undefined) {
            throw new Error('port undefined')
        }

        return this.#port
    }

    getBaseUrl() {
        if(this.#baseUrl === undefined) {
            throw new Error('base url undefined')
        }

        return this.#baseUrl
    }

    isAsRouterModule() {
        return this.#asRouterModule
    }
}

class TelegramOptions {
    #token

    constructor(token) {
        this.#token = token;
    }

    setToken(token) {
        this.#token = token ?? process.env.TG_TOKEN ?? throw new Error('telegram token undefined')
    }

    getToken() {
        if(this.#token === undefined) {
            throw new Error('token undefined')
        }

        return this.#token
    }
}

export { WebOptions, TelegramOptions }