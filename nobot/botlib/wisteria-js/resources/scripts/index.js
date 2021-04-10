/*
     Copyright (c) 2021 Bohdan Sokolovskyi
     Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>
 */

function outputServerStatusResponse(response) {
    document.getElementById('output-server-response').innerText =
        `Status: ${response.status ?? response.message ?? response}`;
}

function outputMessages(from, messages) {
    let area = document.getElementById('bot-dialog-output');
    let lines = area.innerText.split('\n');

    for(let msg of messages) {
        lines.push(`<${from}>: ${msg}`);
    }

    area.innerText = lines.join('\n');
}

function updateCountOfMessagesFromBot(count) {
    let counter = document.getElementById('cnt-of-msgs-from-bot');
    counter.innerText = (count + parseInt(counter.innerText)) + '';

    updateCountOfMsgsFromAll();
}

function updateCountOfMessagesFromUser(count) {
    let counter = document.getElementById('cnt-of-msgs-from-user');
    counter.innerText = (count + parseInt(counter.innerText)) + '';

    updateCountOfMsgsFromAll();
}

function updateCountOfMsgsFromAll() {
    let countFromUser = parseInt(document.getElementById('cnt-of-msgs-from-user').innerText);
    let countFromBot = parseInt(document.getElementById('cnt-of-msgs-from-bot').innerText);

    document.getElementById('cnt-of-msgs-from-all').innerText = (countFromBot + countFromUser) + '';
}


window.onload = () => {
    document.getElementById('bot-dialog-send').onclick = async function() {
        let msg = document.getElementById('bot-dialog-textarea').innerText;

        //TODO: bug with empty str
        if(msg.trim() === "") {
            //TODO: set message about send data is empty
            outputServerStatusResponse('sent empty message');
            return;
        }

        outputMessages("User", [msg]);
        updateCountOfMessagesFromUser(1);

        let jsonResponse;

        try {
            let response = await fetch(
                //window.location.href,
                'http://localhost:3000/',
                {
                    method: 'POST',
                    headers: {
                        'Accept': 'middleware/json',
                        'Content-Type': 'middleware/json'
                    },
                    body: JSON.stringify({
                        msg: msg
                    })
                }
            );

            jsonResponse = await response.json();

            outputServerStatusResponse(response);
            outputMessages(jsonResponse.botName, jsonResponse.messages);
            updateCountOfMessagesFromBot(jsonResponse.messages.length);
        } catch (err) {
            outputServerStatusResponse(err);
        }
    }
}