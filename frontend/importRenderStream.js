function importRenderStream(initialState, render, {
  flyd,
  exchange: { sendMessage },
}) {
  const updateState = flyd.stream(v => v);
  const scannedState = flyd.scan(
    (state, updateF) => updateF(state),
    initialState,
    updateState);

  return scannedState.map(state => {
    render(['div', [
        ['ul#messages'],
        ['input', {
          oncreate({ dom }) {
            dom.focus();
          },
          onkeyup(e) {
            if (e.key === 'Enter') {
              sendMessage(state.chat.selectedRooms[0], e.currentTarget.value);
              e.currentTarget.value = '';
            }
          },
        }]
      ]]);
  });
}
