May need to set a flag for a user_spawn to terminate

Work out if terminate in user_spawn is needed

User pool may not be interacting with WS currrently.

Only interaction seems to be with channel manager

# How WS updates
handle_cast functions are called when js sends a message to the server. 

ws_handles to register callbacks to channel_manager

Pid ! {data} - pass to the process, goes to _info functions not handles

in ws_handler.erl websocket_info just returns back the text passed in.

## Channel Manager

Stores channeluuid and ws_pid_list which have the list of websocket clients (pids).

### Future
Have own channel spawner in future. When the chat/channel dies, it will not affect the other channel processes. (Same design as users).



# Connecting
One websocket connection per user.- this can then join/unjoin various chat rooms.

Need to add a heartbeat.


## TODO:
[] - Make multiple chat rooms
[] - Add heartbeat
[] - Plan out design
[] - Learn a bit more about supervisors/pools (me)

# Running
```bash
# Setup darkhttpd
git clone https://github.com/emikulic/darkhttpd --depth=1 ~/darkhttpd
cd darkhttpd
make
sudo cp darkhttpd /usr/local/bin
rm -rf ~/darkhttpd
```

```bash
# Run frontend with darkhttpd
darkhttpd ./frontend --port 8080
```

## Timesheet

| Date | Time | Description |
| --- | --- | --- |
| 2023-10-09 | 1-2 AM | Setup project - added hot reload with sync package, ping heartbeat, gitignore, and chat\| in response for differentiating between events client side |