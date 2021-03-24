//FIXME: not work
window.onkeypress = (e) => {
    
    // if clicked enter
    if(e.keyCode == 13) {
        document.getElementById('bot-dialog-send').click();
    }
}

document.getElementById('bot-dialog-textarea').onfocus = (e) => {
    
}