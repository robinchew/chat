function importChatRoomViewMulti({ changeView, updateState, channelKey }) {
  return {
    render({ state, exchange }) {
      return ['div', [
        ['h2', { style: { margin: 0 } }, 'Messages for room: ' + channelKey],
        ['ul#messages',
          state.chat.rooms[channelKey].messages.map(message =>
            ['li', message])],
        ['input', {
          oncreate({ dom }) {
            dom.focus();
          },
          onkeyup(e) {
            if (e.key === 'Enter') {
              exchange.sendMessage(channelKey, e.currentTarget.value);
              e.currentTarget.value = '';
            }
          },
        }]
      ]];
    },
  };
}
