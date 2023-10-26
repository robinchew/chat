function importExampleView({ updateState }) {
  return {
    render({ state }) {
      return ['div',
        ['h1', 'Example Heading'],
        ['pre', JSON.stringify(state.route.params)]];
    },
  };
}
