import express from 'express'
import path from 'path'
import { debug, error } from '../../utils/logger.js'

const __dirname = path.resolve();

export class ExpressServer {
    #app
    #port
    #host
    #dirToStaticResources
    #baseUrl = '/'

    constructor(port, host) {
        this.#port = process.env.PORT ?? port ?? 8082
        this.#host = process.env.HOST ?? host ?? 'localhost'
        this.#app = express()
    }

    setBaseUrl(baseUrl) {
        this.#baseUrl = baseUrl;
    }

    setDirOfStaticResources(dir) {
        this.#dirToStaticResources = dir ?? path.resolve(__dirname, '..', '..', 'resources');
        this.#app.use(express.static(dir))
    }

    run() {
        this.#configure()
        this.#app.listen(this.#port, this.#host, (err) => {
            if(err) {
                error(err);
            }

            debug(`server started on host ${this.#host}, on port ${this.#port}`)
        })
    }

    #configure() {
        this.#app.use(express.json())
        this.#app.use(express.static(this.#dirToStaticResources))
    }
}