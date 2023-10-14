function importChatRoomView() {
  return {
    render({ state, exchange }) {
      return ['div', [
        ['ul#messages'],
        ['input', {
          oncreate({ dom }) {
            dom.focus();
          },
          onkeyup(e) {
            if (e.key === 'Enter') {
              exchange.sendMessage(state.chat.selectedRooms[0], e.currentTarget.value);
              e.currentTarget.value = '';
            }
          },
        }]
      ]];
    },
  };
}
