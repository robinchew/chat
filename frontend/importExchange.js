function importExchange({ window }) {
  const resolves = {};
  return {
    init(url, { onOpen, onMessage }) {
      const ws = new WebSocket(url);

      function subscribe() {
        ws.send('subscribe');
      }
      function joinChat(chatName) {
        ws.send(['chat', chatName, 'join'].join('|'));
      }
      function sendMessage(chatName, message) {
        ws.send(['chat', chatName, 'message', message].join('|'));
      }
      function renderMessage(message) {
        const messages = document.getElementById('messages');
        const li = document.createElement('li')
        // li.innerHTML = message;
        // messages.appendChild(li);
      }
      ws.onopen = () => {
        subscribe();
        (onOpen || (() => {}))();
        window.setInterval(
            () => { ws.send('ping') },
            1000 * 55);
      };
      ws.onclose = () => {
          renderMessage('<span style="color:red">Connection closed</span>');
      }
      ws.onmessage = (d) => {
        onMessage(d);
      }
      return {
        joinChat,
        sendMessage,
        subscribe,
      };
    },
  };
}
