var ws = new WebSocket('ws://localhost:3000');

ws.onerror = function () {
    document.getElementById('error').innerHTML = "connection error";
};

ws.onmessage = function (e) {
    document.getElementById('message').innerHTML =
        e.data + '<br>' + document.getElementById('message').innerHTML;
};

var form = document.getElementById('input');

form.addEventListener("submit", function (evt) {
    var text = document.getElementById('text').value;
    if (text.length > 0) {
        ws.send(text);
        document.getElementById('text').value = "";
    }
    evt.preventDefault();
});
