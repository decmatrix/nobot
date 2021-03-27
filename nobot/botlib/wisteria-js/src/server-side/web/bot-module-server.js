import express from 'express';

export class BotExpressModule {
    #baseUrl

    constructor(baseUrl='/bot') {
        this.#baseUrl = `${baseUrl}:msg`
    }

    makeRouterModuleFrom() {
        let botModule = new express.Router()
    }

    injectModule(app, url) {
        app.use(url, this.#botModule)
    }

}