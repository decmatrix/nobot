function outputMessages(from, messages) {
    let area = document.getElementById('bot-dialog-output');
    let lines = area.innerText.split('\n');

    for(let msg of messages) {
        lines.push(`<${from}>: ${msg}`);
    }

    area.innerText = lines.join('\n');
}

function updateCountOfMsgsFromBot(count) {
    let counter = document.getElementById('cnt-of-msgs-from-bot');
    counter.innerText = (count + parseInt(counter.innerText)) + '';

    updateCountOfMsgsFromAll();
}

function updateCountOfMsgsFromUser(count) {
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
        if(msg === "\n") {
            //TODO: set message about send data is empty
            return;
        }

        outputMessages("User", [msg]);
        updateCountOfMsgsFromUser(1);

        let response = await fetch(
            //window.location.href, 
            'http://localhost:3000/',
            {
                method: 'POST',
                headers: {
                    'Accept': 'application/json',
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    msg: msg
                })
            }
            );
        let jsonResponse = await response.json();
        
        outputMessages(jsonResponse.botName, jsonResponse.messages);
        updateCountOfMsgsFromBot(jsonResponse.messages.length);
    }
}