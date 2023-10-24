function importRenderStream(flyd, initialState) {
  const updateState = flyd.stream(v => v);
  return {
    updateState,
    stream: flyd.scan(
      (state, updateF) => updateF(state),
      initialState,
      updateState),
  }
}
