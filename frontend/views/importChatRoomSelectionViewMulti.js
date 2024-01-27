// list of rooms that can be clicked,
// on click, log the value of the room (uuid)
// state.chat.rooms is an object with keys for each room
const importChatRoomSelectionViewMulti = (importChatRoomViewMulti) => (({
  ramda: {
    pipe,
  },
  changeView,
  updateState,
}) => {
  return {
    render({ state, exchange }) {
      const rooms = Object.keys(state.chat.rooms).map(function (channelKey) {
        console.log(channelKey);
        return importChatRoomViewMulti({
          ramda: {
            pipe,
          },
          changeView,
          updateState,
          channelKey
        })
      });
      return ['div',
        ['p',
          {
            onclick(e) {
              e.preventDefault();
              updateState(changeView('channels'));
            },
          },
          'Room'],
        ['h1', {}, 'Multi Room View'],
        ['h2', { style: { margin: 0 } }, 'Select a room'],
        [
          ['ul#rooms',
            Object.keys(state.chat.rooms).map((room) => {
              return ['li', { onclick: () => {
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
          }],
          rooms.map(room => room.render({state,exchange}))
        ]
      ];
    }
  };
});
