# disco.el

`disco.el` is an Emacs Discord client project.

This repository currently contains an MVP scaffold designed with these references in mind:

- `telega.el`: list-driven root buffer UX and Emacs-native command flow.
- `ement.el`: modular split between API layer, state model, and room buffers.
- `discord-userdoccers`: endpoint/gateway behavior and rate-limit notes.

## Current MVP Scope

- Configure token with `DISCO_TOKEN` environment variable (default), or override in-session with `M-x disco-set-token`.
- Start client with `M-x disco`.
- Fetch and display guild/channel list in `*disco*`.
- Fetch and display private channels (DM/group DM) in root.
- Hide guild channels lacking `VIEW_CHANNEL` based on computed channel permissions.
- Fetch and display thread channels nested under their parent channels.
- Browse archived thread lists per parent channel.
- Open channel timeline.
- Send plain text message with `C-c C-c` in room buffer.
- Reply/edit/delete message from room buffer (`r`, `e`, `d`) and load older history (`M-<`).
- In parent channel rooms: create thread from message (`C-c C-t m`) or detached thread (`C-c C-t c`).
- In thread rooms: join (`C-c C-j`), leave (`C-c C-l`), toggle archived (`C-c C-a`).
- Live room updates with create/update/delete dispatch from Discord Gateway websocket events.
- Root buffer now live-syncs guild/channel/thread structure from gateway create/update/delete dispatch.
- Root channel labels include lightweight unread counters from live message events.
- Room open/refresh/live message flow now acknowledges Discord channel read-state (`/ack`) and tracks per-channel last-read cursor.
- Room buffers update on channel/thread rename/state change and auto-close when backing channel/guild is deleted.
- Gateway READY now ingests private channel payload and keeps local DM list in sync.
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
- `M-x disco-describe-gateway`: show websocket state, watched channel count, resume/session info, and reconnect backoff state.

## Thread Commands

- Root buffer: `A` opens archived thread browser for a selected parent channel.
- Archived thread buffer: `g` refreshes from first page, `n` loads next page, `RET`/mouse opens selected thread.
- Thread room buffer: `C-c C-j` join, `C-c C-l` leave, `C-c C-a` toggle archived state.
- Parent room buffer: `C-c C-t m` creates from message, `C-c C-t c` creates detached thread.
- Room transient (`?`): includes message send/refresh, thread create/join/leave/archive, and inspect actions.

## Message Commands

- Room buffer: `r` set reply target from message-at-point, `C-c C-k` clears pending reply.
- Room buffer: `e` edits message-at-point, `d` deletes message-at-point (with confirmation).
- Room buffer: `M-<` loads older message page using `before` cursor pagination.
- Room transient (`?`): includes load older / reply / cancel reply / edit / delete actions.
- Root channel labels show `[read]` when local read cursor reaches known channel `last_message_id`.

## Gateway Configuration

- `disco-gateway-version`: gateway API version (default now aligned to `v10`).
- `disco-gateway-transport-compression`: optional transport compression (`zlib-stream` or disabled).
- `disco-gateway-zlib-max-buffer-bytes`: safety cap for accumulated compressed stream bytes.
- `disco-gateway-identify-intents`: optional identify intents bitmask.
- `disco-gateway-identify-capabilities`: optional identify capabilities bitmask.
- `disco-gateway-identify-presence`: optional identify presence object (alist).
- `disco-fetch-guild-active-threads`: optionally fetch `/guilds/{id}/threads/active` during root refresh.
- `disco-thread-archive-fetch-limit`: page size used by archived thread fetchers (2-100).
- `disco-gateway-reconnect-delay`: base reconnect delay.
- `disco-gateway-max-reconnect-attempts`: hard cap for consecutive reconnects (`nil` for unlimited).
- `disco-gateway-reconnect-max-delay`: max reconnect delay cap.
- `disco-gateway-reconnect-multiplier`: exponential backoff multiplier.
- `disco-gateway-reconnect-jitter`: reconnect delay randomization ratio.
- `disco-gateway-invalid-session-min-delay` / `disco-gateway-invalid-session-max-delay`: randomized reconnect window for opcode 9.
- `disco-user-agent`: HTTP `User-Agent` header (default is desktop-style Discord/Electron shape).

## Design Notes

- Initial implementation intentionally keeps synchronous request flow to simplify debugging and establish API correctness first.
- HTTP transport is fully based on `plz` (curl-backed), with optional in-process serialization to avoid burst traffic.
- REST calls apply rate-limit coordination (global + bucket/route cooldown) and bounded 429 retries.
- Live updates use real Discord Gateway websocket flow (`HELLO`/heartbeat/identify/resume) and dispatch message events through a stable local hook contract.
- Gateway dispatch now also mutates and emits channel/guild/thread structural events for live UI consistency.
- Gateway `READY` read-state payload and `MESSAGE_ACK` dispatch update local read cursors/unread mentions.
- Gateway transport supports optional `compress=zlib-stream` and decodes binary payloads with a per-connection shared stream context.
- Thread channels are indexed by parent channel, rendered hierarchically in root, and updated from `THREAD_CREATE`/`THREAD_UPDATE`/`THREAD_DELETE`/`THREAD_LIST_SYNC` gateway events.
- Gateway reconnect uses exponential backoff with jitter for transport failures, and randomized delay handling for `INVALID_SESSION`.
- Identify payload supports optional intents/capabilities/presence fields through customization.
- Rate-limit handling currently surfaces 429 with retry metadata to the user; full bucket scheduler is planned next.

## Next Milestones

1. Expand dispatch handling beyond messages/threads (channel/guild mutations and unread state).
2. Improve root/room rendering (unread markers, compact mode, keyboard navigation parity with telega-style workflows).
3. Add queue prioritization/backpressure so user actions are favored over background work.
4. Add persisted session recovery (resume state restore across Emacs restarts).
