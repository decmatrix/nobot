const express = require('express');
const fetch = require('node-fetch');
const app = express();
const port = 3000;
const url = 'https://api.telegram.org/bot';
const botToken = '1783807678:AAGHn8spvjEa8SJzR9oJeNcNeYIRQgI3ldw';

app.use(express.json());

app.post('/', (req, res) => {
    console.log("Here");
    let chatId = req.body.message.chat.id;
    let sentMessage = req.body.message.text;

    if(sentMessage.match(/hello/gi)) {
        fetch(`${url}${botToken}/sendMessage`, {
            method: 'POST', body: {
                chat_id: chatId,
                text: 'Hello back'
            }
        }).then((response) => {
            console.log(response);
            res.status(200).send(respone);
        }).catch((err) => {
            console.log(err);
            res.send(err);
        });
    } else {
        res.status(200).send({});
    }
});

app.listen(port, () => {
    console.log('Server started on port 80');
});

// const Telegraf = require('telegraf');
// const app = new Telegraf.Telegraf('1783807678:AAGHn8spvjEa8SJzR9oJeNcNeYIRQgI3ldw');

// app.hears('hi', ctx => {
//  return ctx.reply('Hey!');
// });

// app.startPolling();