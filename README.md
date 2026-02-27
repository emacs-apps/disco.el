# disco.el

`disco.el` is an Emacs Discord client project.

This repository currently contains an MVP scaffold designed with these references in mind:

- `telega.el`: list-driven root buffer UX and Emacs-native command flow.
- `ement.el`: modular split between API layer, state model, and room buffers.
- `discord-userdoccers`: endpoint/gateway behavior and rate-limit notes.

## Current MVP Scope

- Configure token in-session with `M-x disco-set-token`.
- Start client with `M-x disco`.
- Fetch and display guild/channel list in `*disco*`.
- Open channel timeline.
- Send plain text message with `C-c C-c` in room buffer.
- Live room updates with create/update/delete dispatch (polling-based gateway-like engine).

## Dependencies

- Emacs 27.1+
- `plz` (required): HTTP transport backend

## File Layout

- `disco.el`: package entrypoint.
- `disco-customize.el`: user options and token command.
- `disco-api.el`: synchronous REST requests.
- `disco-http.el`: HTTP wrapper built on required `plz` backend.
- `disco-state.el`: in-memory guild/channel/message cache.
- `disco-gateway.el`: live update engine and event dispatch hook.
- `disco-root.el`: root dashboard buffer.
- `disco-room.el`: room buffer render/send/refresh flow.

## Design Notes

- Initial implementation intentionally keeps synchronous request flow to simplify debugging and establish API correctness first.
- HTTP transport is fully based on `plz` (curl-backed).
- Live updates currently use polling and emit gateway-like message events; full WebSocket Gateway transport is the next protocol milestone.
- Rate-limit handling currently surfaces 429 with retry metadata to the user; full bucket scheduler is planned next.

## Next Milestones

1. Add true Gateway websocket lifecycle (`/gateway`, Hello/Heartbeat/Identify/Resume).
2. Replace polling transport with Gateway dispatch stream while keeping current event hook API.
3. Improve root/room rendering (unread markers, compact mode, keyboard navigation parity with telega-style workflows).
4. Introduce async request queue + rate-limit bucket management.
