function importChatRoomView({ changeView, updateState }) {
  return {
    render({ state, exchange }) {
      return ['div', [
        ['p',
          {
            onclick(e) {
              e.preventDefault();
              updateState(changeView('channels'));
            },
          },
          'Room'],
        ['h2', { style: { margin: 0 } }, 'Messages for room: ' + state.route.params.channel_key],
        ['ul#messages',
          state.chat.rooms[state.route.params.channel_key].messages.map(message =>
            ['li', message])],
        ['input', {
          oncreate({ dom }) {
            dom.focus();
          },
          onkeyup(e) {
            if (e.key === 'Enter') {
              exchange.sendMessage(state.route.params.channel_key, e.currentTarget.value);
              e.currentTarget.value = '';
            }
          },
        }]
      ]];
    },
  };
}
