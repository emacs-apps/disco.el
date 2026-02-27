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
- Live room updates with create/update/delete dispatch from Discord Gateway websocket events.
- Request serialization and rate-limit-aware retries for Discord REST calls.

## Dependencies

- Emacs 27.1+
- `plz` (required): HTTP transport backend
- `websocket` (required): Discord Gateway websocket transport

## File Layout

- `disco.el`: package entrypoint.
- `disco-customize.el`: user options and token command.
- `disco-api.el`: synchronous REST requests.
- `disco-http.el`: synchronous HTTP wrapper on `plz` with serialized request queue.
- `disco-state.el`: in-memory guild/channel/message cache.
- `disco-gateway.el`: Discord Gateway websocket transport and dispatch hook.
- `disco-root.el`: root dashboard buffer.
- `disco-room.el`: room buffer render/send/refresh flow.

## Runtime Observability

- `M-x disco-describe-http-queue`: show current queue limit/active/pending counts.
- `M-x disco-describe-rate-limits`: open a buffer showing global/route/bucket cooldown state.
- `M-x disco-describe-gateway`: show websocket state, watched channel count, and resume/session info.

## Design Notes

- Initial implementation intentionally keeps synchronous request flow to simplify debugging and establish API correctness first.
- HTTP transport is fully based on `plz` (curl-backed), with optional in-process serialization to avoid burst traffic.
- REST calls apply rate-limit coordination (global + bucket/route cooldown) and bounded 429 retries.
- Live updates use real Discord Gateway websocket flow (`HELLO`/heartbeat/identify/resume) and dispatch message events through a stable local hook contract.
- Rate-limit handling currently surfaces 429 with retry metadata to the user; full bucket scheduler is planned next.

## Next Milestones

1. Improve gateway resiliency (exponential reconnect backoff, richer invalid-session handling, startup race hardening).
2. Expand dispatch handling beyond message events (channel/guild mutations and unread state).
3. Improve root/room rendering (unread markers, compact mode, keyboard navigation parity with telega-style workflows).
4. Add queue prioritization/backpressure so user actions are favored over background work.
