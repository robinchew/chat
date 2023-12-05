// list of rooms that can be clicked,
// on click, log the value of the room (uuid)
// state.chat.rooms is an object with keys for each room
function importChatRoomSelectionView({
  ramda: {
    pipe,
  },
  changeView,
  updateState
}) {
    return {
        render({ state, exchange }) {
            return ['div',
                ['h2', { style: { margin: 0 } }, 'Select a room'],
                [
                    ['ul#rooms',
                        Object.keys(state.chat.rooms).map((room) => {
                            return ['li', { onclick: () => {
                                // change room
                                updateState(changeView('channel', {
                                  channel_key: room,
                                }));
                                // subscribe to room
                                exchange.joinChat(room);
                            }}, room];
                        })
                    ],
                    // input to make a new room
                    ['input', {
                        onkeyup(e) {
                            if (e.key === 'Enter') {
                                updateState(pipe(
                                  changeView('channel', {
                                    channel_key: e.currentTarget.value,
                                  }),
                                  state => O(state, {
                                      chat: O({
                                          rooms: O({
                                              [e.currentTarget.value]: {
                                                  messages: []
                                              }
                                          })
                                      })
                                  })));
                                exchange.joinChat(e.currentTarget.value);
                                e.currentTarget.value = '';
                            }
                        },
                    }]
                ]
            ];
        }
    };
}
