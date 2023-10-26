function importChatRoomView({ changeView, updateState }) {
  return {
    render({ state, exchange }) {
      return ['div', [
        ['a',
          {
            href: '',
            onclick(e) {
              e.preventDefault();
              updateState(changeView('channels'));
            },
          },
          'Room'],
        ['h2', { style: { margin: 0 } }, 'Messages for room: ' + state.chat.selectedRoom],
        ['ul#messages',
          state.chat.rooms[state.chat.selectedRoom].messages.map(message =>
            ['li', message])],
        ['input', {
          oncreate({ dom }) {
            dom.focus();
          },
          onkeyup(e) {
            if (e.key === 'Enter') {
              exchange.sendMessage(state.chat.selectedRoom, e.currentTarget.value);
              e.currentTarget.value = '';
            }
          },
        }]
      ]];
    },
  };
}
