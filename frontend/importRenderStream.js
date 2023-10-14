function importRenderStream({ exchange: { sendMessage } }) {
  return render => render(['div', [
    ['h1', 'hhhu'],
    ['ul#messages'],
    ['input', {
      oncreate({ dom }) {
        dom.focus();
      },
      onkeyup(e) {
        if (e.key === 'Enter') {
          sendMessage('room1_UUID', e.currentTarget.value);
          e.currentTarget.value = '';
        }
      },
    }]
  ]]);
}
