// list of rooms that can be clicked,
// on click, log the value of the room (uuid)
// state.chat.rooms is an object with keys for each room
function importChatRoomSelectionView({ updateState }) {
    return {
        render({ state, exchange }) {
            return ['div', 
                ['h2', { style: { margin: 0 } }, 'Select a room'],
                [
                    ['ul#rooms',
                        Object.keys(state.chat.rooms).map((room) => {
                            return ['li', { onclick: () => {
                                // change room
                                updateState(state => O(state, {
                                    chat: O({}, state.chat, {
                                        selectedRoom: room
                                    })
                                }));
                                // subscribe to room
                                // exchange.joinChat(room);
                            }}, room];
                        })
                    ],
                    // input to make a new room
                    ['input', {
                        onkeyup(e) {
                            if (e.key === 'Enter') {
                                console.log(e.currentTarget.value);
                                updateState(state => O(state, {
                                    chat: O({}, state.chat, {
                                        selectedRoom: e.currentTarget.value,
                                        rooms: O({}, state.chat.rooms, {
                                            [e.currentTarget.value]: {
                                                messages: []
                                            }
                                        })
                                    })
                                }));
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
            