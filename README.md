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
- Fetch and display private channels (DM/group DM/ephemeral DM) in root.
- Hide guild channels lacking `VIEW_CHANNEL` based on computed channel permissions.
- Fetch and display thread channels nested under their parent channels.
- Browse archived thread lists per parent channel.
- Open channel timelines, including voice/stage text chats; forum/media parents open thread browsers; directory/lobby rows open inspect views.
- Room timeline supports telega-inspired compact same-sender grouping, date separators, and unread divider rendering.
- Room message rows now use a telega-like two-line feel: author/avatar header with right-aligned time, plus indented body/reply continuation lines for grouped messages.
- Send plain text message with `C-c C-c` in room buffer.
- Send file attachments from room buffer (multipart upload) with draft tokens: add via `C-c C-f`, remove token at point via `C-c C-d`, clear all via `C-c C-x`, list/edit/reorder via `C-c M-l`/`C-c M-e`/`C-c M-r`, then send via `RET`/`C-c C-c`.
- Reply/edit/delete/forward message from room buffer (`r`, `e`, `d`, `C-c C-F`) and load older history (`M-<`).
- Message rows with starter threads show `[Open thread]`; use `C-c C-t o` at message point to jump to the corresponding thread room.
- Message rows render telega-inspired rich attachment cards (kind/name/meta, caption, URL actions, and transfer state actions: Download/Cancel/Open Local/Save As, plus inline image preview loading).
- Message rows render telega-inspired rich embed cards (title/meta, description/fields/footer, URL/media actions, and inline embed-image preview loading).
- Message rows render reaction chips, with reaction operations at point (`!` toggle, `+` add, `-` remove).
- Message rows render poll blocks with staged answer selection, explicit submit/remove/end controls, and live vote-count updates from gateway poll vote events.
- Room composer supports direct inline typing after `>>>`, with `C-c '` edit, `M-p/M-n` draft history, and `RET` quick send.
- Room prompt footer now shows telega-style live typing indicators when other users are typing (DM + guild channels).
- Room prompt/history are immutable while only the draft area after `>>>` is editable (telega-style input boundary).
- Room keyboard search flow (`s` then `n`/`p`) for message-level navigation.
- Room message rows now include reply preview lines, avatar placeholders, and deterministic multi-color author names.
- Room can render inline Discord avatar images (async, cached) with automatic placeholder fallback.
- Draft input supports dynamic `TAB` completion for `@`/`#` tokens (users, roles, `@everyone`/`@here`, and guild channels) and inserts Discord mention syntax; company/Corfu rows can show username/id/avatar metadata.
- Room provides `C-c C-v` to clear avatar cache and refetch avatars when image decoding/network glitches occur.
- In parent channel rooms: create thread from message (`C-c C-t m`) or detached thread (`C-c C-t c`).
- In thread rooms: join (`C-c C-j`), leave (`C-c C-l`), toggle archived (`C-c C-a`), rename/lock/slowmode/auto-archive/settings (`C-c C-t ...`), and set current-user mute (`C-c C-t u`).
- Live room updates with create/update/delete dispatch from Discord Gateway websocket events.
- Root buffer now live-syncs guild/channel/thread structure from gateway create/update/delete dispatch.
- Root channel labels show unread state plus mention badges (`[@N]`), aligned with Discord read-state semantics (not per-channel unread message counts).
- Root rows can append custom display fragments via `disco-root-extra-info-functions` (channel/guild/category providers).
- Room open/refresh/live message flow now acknowledges Discord channel read-state (`/ack`) and tracks per-channel last-read cursor.
- Root and room refresh paths are now asynchronous to avoid blocking Emacs UI.
- Send/edit/delete in room use asynchronous REST requests to reduce command-time blocking.
- Room buffers update on channel/thread rename/state change and auto-close when backing channel/guild is deleted.
- Gateway READY now ingests private channel payload and keeps local DM list in sync.
- Root navigation adds telega-style keyboard flow (`n`/`p`/`TAB`, `RET`, `u`) plus layout cycle (`l`), explicit layout selection (`L`), sort toggle (`\`), view cycle (`v`: all/unread/dms), and unread-lens toggle (`U`).
- Root now supports multiple layouts: telega-style activity list (non-collapsible, one-line rows as `<icon> [channel | category | guild] <preview> <time>`) and collapsible tree browse, with user-defined custom layouts via layout specs.
- Activity layout excludes thread rows by default to keep large guilds responsive; enable them with `disco-root-activity-include-threads` when needed.
- Root rendering uses EWOC plus debounced live-update aggregation; tree and activity layouts now patch rows incrementally, with activity row reordering to track sort changes without full buffer rebuilds.
- Room timeline rendering now uses EWOC, with local message-row refresh on live create/update/delete events.
- Request serialization and rate-limit-aware retries for Discord REST calls.

## Dependencies

- Emacs 27.1+
- `plz` (required): HTTP transport backend
- `websocket` (required): Discord Gateway websocket transport

## File Layout

- `disco.el`: package entrypoint.
- `disco-customize.el`: user options and token command.
- `disco-ui.el`: shared UI rendering primitives (buttons, styled lines, list sections).
- `disco-api.el`: REST requests (sync + async callback paths).
- `disco-http.el`: HTTP wrapper on `plz` (sync + async queue-backed paths).
- `disco-permission.el`: shared Discord permission bitfield constants/parsing/check helpers.
- `disco-state.el`: in-memory guild/channel/message cache.
- `disco-gateway.el`: Discord Gateway websocket transport and dispatch hook.
- `disco-root-layout.el`: root layout registry, view-spec composition, built-in layout specs, and user-defined layout customization.
- `disco-view.el`: shared cursor preservation helpers plus reusable one-line/list-view rendering helpers.
- `disco-root.el`: root dashboard buffer controllers and root/thread-browser view orchestration.
- `disco-room.el`: room buffer render/send flow with async refresh/pagination.
- `disco-company.el`: composer completion engine (`@`/`#`, CAPF, optional company backend).

## Custom Layout Example

Use `:build` when you want a custom layout to return a view spec instead of
rendering buffer contents directly. For simple custom layouts, pair `:build`
with `:update-mode full` and return a `list-spec`:

```elisp
(defun my-disco-root-build-dm-focus ()
  (let ((channels (disco-root--visible-private-channels)))
    (disco-root-layout-view-spec-create
     :kind 'list-spec
     :list-spec
     (disco-view-list-spec-create
      :title "DM Focus"
      :summary (format "Visible DMs: %d" (length channels))
      :items channels
      :item-inserter (lambda (channel)
                       (disco-root--insert-channel-line channel 2 'activity))
      :empty-text "(no visible private channels)"))))

(setq disco-root-custom-layouts
      '((dm-focus
         :label "DM Focus"
         :build my-disco-root-build-dm-focus
         :update-mode full
         :unread-mode filter
         :toggle-hint "next channel")))
```

## Runtime Observability

- `M-x disco-describe-http-queue`: show current queue limit/active/pending counts.
- `M-x disco-describe-rate-limits`: open a buffer showing global/route/bucket cooldown state.
- `M-x disco-describe-gateway`: show websocket state, watched channel count, resume/session info, and reconnect backoff state.

## Thread Commands

- Root buffer: `A` opens archived thread browser for a selected parent channel.
- Root buffer: `RET` on forum/media opens parent-thread list; that list fetches active threads via `/channels/{id}/threads/search` (`archived=false`) on open and on `g`.
- Archived thread buffer: `g` refreshes from first page, `n` loads next page, `RET`/mouse opens selected thread.
- Archived thread fetch only queries the `private` source when `MANAGE_THREADS` is present, and also suppresses expected permission-denied (`Missing Access`) source noise.
- Thread room buffer: `C-c C-j` join, `C-c C-l` leave, `C-c C-a` toggle archived state.
- Thread room buffer: `C-c C-t r` rename, `C-c C-t k` toggle locked, `C-c C-t s` set slowmode, `C-c C-t a` set auto-archive duration, `C-c C-t e` edit multi-field settings, `C-c C-t u` set thread mute.
- Parent room buffer: `C-c C-t m` creates from message, `C-c C-t c` creates detached thread.
- Room transient (`?`): includes message send/refresh plus thread create/join/leave/archive/rename/lock/slowmode/auto-archive/settings/mute actions.

## Message Commands

- Room buffer: `r` set reply target from message-at-point, `C-c C-k` clears pending reply.
- Room buffer: `C-c C-F` forwards a message by id/channel, with optional comment and optional forward-only subset (`embed_indices` / `attachment_ids`) chosen from source message entries. In picker prompts, press `RET` on empty input to skip one side. Manual fallback is off by default (`disco-room-forward-only-manual-fallback`).
- If API rejects forward comments in your session, `disco-room-forward-comment-rejection-action` controls fallback (`split` sends comment + forward as two messages).
- Room buffer: `e` edits message-at-point, `d` deletes message-at-point (with confirmation).
- Room buffer: `M-<` loads older message page using `before` cursor pagination.
- Room buffer draft: attachment tokens can be removed at point with `C-c C-d`.
- Room transient (`?`): includes load older / reply / cancel reply / edit / delete actions.
- Room poll actions: `C-c C-p s` send poll, `C-c C-p +` select answer, `C-c C-p -` unselect answer, `C-c C-p t` toggle staged answer, `C-c C-p v` submit staged vote, `C-c C-p c` remove own vote, `C-c C-p e` end poll.
- Room transient (`?`): includes attachment/forward and reaction/poll actions (`f`, `F`, `D`, `x`, `v`, `V`, `O`, `!`, `+`, `-`, `p`, `w`, `u`, `t`, `W`, `C`, `X`).
- Room transient (`?`): thread section includes create/open/manage actions (`m`, `o`, `n`, `R`, `L`, `S`, `U`, `E`, `M`, `j`, `l`, `a`, `A`).
- Mention send policy can be tuned via `disco-room-allowed-mentions` and `disco-room-reply-mention-replied-user`.
- `disco-room-enable-company-backend` controls optional company integration for composer completion (`disco-room-company-completion`); `disco-company-show-user-avatars` toggles avatar rendering, and `disco-company-capf-avatar-size` keeps completion row height stable for both Corfu/CAPF and company.
- Root channel labels show `[read]` when local read cursor reaches known channel `last_message_id`.
- `disco-root-live-update-debounce` controls how quickly aggregated gateway bursts flush into incremental root patches.
- `disco-root-activity-header-refresh-interval` throttles implicit activity header refreshes during message bursts.
- `disco-root-default-layout`, `disco-root-custom-layouts`, `disco-root-tree-default-show-unread-section`, and `disco-root-tree-unread-section-limit` control root layout behavior; custom layouts can now provide either legacy `:render` handlers or `:build` view-spec builders.
- `disco-root-activity-context-width` controls the left context block width in activity rows (telega-like fixed/ratio/bounded semantics).
- `disco-root-activity-include-threads` controls whether thread channels are listed in activity layout (default off for performance).
- `disco-root-activity-time-format-alist` and `disco-root-week-start-day` control telega-like activity timestamp formatting buckets.
- `disco-root-auto-fill-on-window-size-change` keeps root rows auto-aligned when window width/text scale changes; `disco-root-auto-fill-margin-columns` reserves extra right margin, and `M-x disco-root-buffer-auto-fill` forces one manual reflow.
- `disco-root-extra-info-functions` lets you inject extra row metadata without blocking network calls in the renderer.

## Gateway Configuration

- `disco-gateway-version`: gateway API version (default now aligned to `v10`).
- `disco-gateway-transport-compression`: optional transport compression (`zlib-stream` or disabled).
- `disco-gateway-zlib-max-buffer-bytes`: safety cap for accumulated compressed stream bytes.
- `disco-gateway-identify-intents`: optional identify intents bitmask.
  - If intents are explicitly set, include `GUILD_MESSAGE_TYPING` (`1<<11`) and/or `DIRECT_MESSAGE_TYPING` (`1<<14`) to receive typing events.
  - Include `GUILD_MESSAGE_POLLS` (`1<<24`) and/or `DIRECT_MESSAGE_POLLS` (`1<<25`) to receive `MESSAGE_POLL_VOTE_ADD` / `MESSAGE_POLL_VOTE_REMOVE` events.
- `disco-gateway-identify-capabilities`: optional identify capabilities bitmask (merged with passive-v2 bit when enabled below).
- `disco-gateway-enable-passive-guild-update-v2`: when non-nil (default), automatically include `PASSIVE_GUILD_UPDATE_V2` capability so activity/root can refresh from passive guild unread deltas without channel-wide subscriptions.
- `disco-gateway-identify-presence`: optional identify presence object (alist).
- `disco-gateway-enable-lazy-channel-subscriptions`: when non-nil, send Gateway op 14 channel subscriptions for watched guild channels (needed in user sessions so guild `TYPING_START` is delivered reliably).
- `disco-fetch-guild-active-threads`: optionally fetch `/guilds/{id}/threads/active` during root refresh (Discord docs mark this route bot-only; user accounts return 403).
- `disco-thread-archive-fetch-limit`: page size used by archived thread fetchers (2-100).
- `disco-gateway-reconnect-delay`: base reconnect delay.
- `disco-gateway-max-reconnect-attempts`: hard cap for consecutive reconnects (`nil` for unlimited).
- `disco-gateway-reconnect-max-delay`: max reconnect delay cap.
- `disco-gateway-reconnect-multiplier`: exponential backoff multiplier.
- `disco-gateway-reconnect-jitter`: reconnect delay randomization ratio.
- `disco-gateway-invalid-session-min-delay` / `disco-gateway-invalid-session-max-delay`: randomized reconnect window for opcode 9.
- `disco-user-agent`: HTTP `User-Agent` header (default is desktop-style Discord/Electron shape).

## Design Notes

- Root/room refresh and message actions use async request flow to keep Emacs responsive during network activity.
- HTTP transport is fully based on `plz` (curl-backed), with optional in-process serialization to avoid burst traffic.
- REST calls apply rate-limit coordination (global + bucket/route cooldown) and bounded 429 retries.
- Live updates use real Discord Gateway websocket flow (`HELLO`/heartbeat/identify/resume) and dispatch message events through a stable local hook contract.
- Gateway dispatch now also mutates and emits channel/guild/thread structural events for live UI consistency.
- Root EWOC state keeps channel-node indexes so message/read events can update rows incrementally.
- Room EWOC state keeps message-node indexes; reaction and poll-vote events patch rows locally while create/update/delete rerender fully to preserve grouping/day/unread layout correctness.
- Shared `disco-ui` primitives are reused by room cards and forum/thread list buffers to keep UI interactions and list layout consistent.
- Avatar fetch/render pipeline is asynchronous and rerenders room buffers when images become available.
- Composer completion is token-boundary aware for `@`/`#`, with dynamic candidate lists and optional company backend integration (`disco-room-company-completion`).
- Gateway `READY` read-state payload and `MESSAGE_ACK` dispatch update local read cursors/unread mentions.
- Gateway transport supports optional `compress=zlib-stream` and decodes binary payloads with a per-connection shared stream context.
- Thread channels are indexed by parent channel, rendered hierarchically in root, and updated from `THREAD_CREATE`/`THREAD_UPDATE`/`THREAD_DELETE`/`THREAD_LIST_SYNC` gateway events.
- Gateway thread membership deltas (`THREAD_MEMBER_UPDATE`/`THREAD_MEMBERS_UPDATE`) now update lightweight per-thread member caches.
- Gateway reconnect uses exponential backoff with jitter for transport failures, and randomized delay handling for `INVALID_SESSION`.
- Identify payload supports optional intents/capabilities/presence fields through customization.
- Rate-limit handling currently surfaces 429 with retry metadata to the user; full bucket scheduler is planned next.

## Next Milestones

1. Add tree interaction controls (collapse/expand guilds and thread subtrees) on top of the EWOC root model.
2. Improve mention/composer parity (`M-r` history search, mention candidate popup UX, optional multiline compose mode).
3. Expand fast navigation (`M-g` prefix map for unread/mentions/reactions style jumps).
4. Add queue prioritization/backpressure so user actions are favored over background work.
