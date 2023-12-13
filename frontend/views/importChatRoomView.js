function importChatRoomView({ changeView, updateState }) {
  return messages => ({
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
        ['h2', { style: { margin: 0 } }, 'Messages for room: ' + state.route.params.channel_key],
        ['ul#messages',
          messages.map(message =>
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
  });
}
