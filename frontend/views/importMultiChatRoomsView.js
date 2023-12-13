const importMultiChatRoomsView = (views) => ({
  ramda: {
    pipe,
  },
  patchinko: O,
}) => {
  return {
    render(...viewArgs) {
      const { state } = viewArgs[0];
      return ['div',
        Object.entries(state.chat.rooms).map(([roomName, { name, messages }]) => {
          return views.chatroom(messages).render(...viewArgs);
        })];
    },
  };
}
