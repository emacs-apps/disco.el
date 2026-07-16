# disco.el

`disco.el` is an Emacs Discord client project.

This repository currently contains an MVP scaffold designed with these references in mind:

- `telega.el`: list-driven root buffer UX and Emacs-native command flow.
- `ement.el`: modular split between API layer, state model, and room buffers.
- `discord-userdoccers`: endpoint/gateway behavior and rate-limit notes.

## Current MVP Scope

- Configure token with `DISCO_TOKEN` environment variable (default), or override in-session with `M-x disco-set-token`.
- Start client with `M-x disco`.
- Display a compact guild navigator in `*disco*`; opening a guild creates a
  dedicated, lazy channel-directory buffer.
- Fetch and display private channels (DM/group DM/ephemeral DM) in root.
- Hide guild channels lacking `VIEW_CHANNEL` based on computed channel permissions.
- Display active thread channels beneath expandable forum/media parents in each
  guild directory.
- Browse archived thread lists per parent channel.
- Open channel timelines, including voice/stage text chats; forum/media parents
  expand in their guild directory; directory/lobby rows open inspect views.
- Room timeline supports telega-inspired compact same-sender grouping, date separators, and unread divider rendering.
- Room message rows now use a telega-like two-line feel: author/avatar header with right-aligned time, plus indented body/reply continuation lines for grouped messages.
- Send plain text message with `C-c RET` in room buffer; `C-c C-c` cancels an active message filter.
- Send file attachments from room buffer (multipart upload) with structured composer objects: telega-like attach menu on `C-c C-a`, direct file attach on `C-c C-f`, reserved clipboard attach slot `C-c C-v`, remove attachment at point via `C-c C-d`, clear all via `C-c C-x`, list/edit/reorder via `C-c M-l`/`C-c M-e`/`C-c M-r`, then send via `RET`/`C-c RET`.
- Reply/edit/delete/forward message from room buffer with telega-like timeline keys `r`/`f`/`e`/`d` when point is outside the composer, or via `C-c m r/f/e/d`; legacy `C-c C-F` still works for forward. History extends automatically near either visible edge, while `M-<`/`M->` retain native Emacs behavior.
- Message rows with starter threads show `[Open thread]`; use `C-c C-t o` at message point to jump to the corresponding thread room.
- Message rows render compact telega-inspired attachment cards: title/preview is the primary open/play action, only meaningful transfer status stays inline, and Download/Cancel/Save As/Copy URL live in the message transient.  Cards carry a backend-neutral action context shared with emacs-qq; audio keeps its stateful play/pause/stop waveform controls inline.
- Message rows render telega-inspired rich embed cards (title/meta, description/fields/footer, URL/media actions, and inline embed-image preview loading).
- Message rows render reaction chips, with reaction operations at point (`!` toggle, `+` add, `-` remove).
- Message rows render poll blocks with staged answer selection, explicit submit/remove/end controls, and live vote-count updates from gateway poll vote events.
- Room composer supports direct inline typing after `>>>`, with `C-c '` edit, `M-p/M-n/M-r` draft history navigation/search, `M-RET` parsed preview, and `RET` quick send.
- Room prompt footer now shows telega-style live typing indicators when other users are typing (DM + guild channels).
- Room prompt/history are immutable while only the draft area after `>>>` is editable (telega-style input boundary).
- Room keyboard search flow (`s` then `n`/`p`) for message-level navigation.
- Room message rows now include reply preview lines, avatar placeholders, and deterministic multi-color author names.
- Room can render inline Discord avatar images (async, cached) with automatic placeholder fallback.
- Draft input supports dynamic `TAB` completion for `@`/`#`/`:` tokens (remote guild-member prefix search, roles, `@everyone`/`@here`, guild channels, standard Unicode emoji, and guild custom emoji) and inserts native Discord mention/emoji syntax; company/Corfu rows can show nickname/global-name/username/id/avatar metadata.
- Room provides `C-c M-v` to clear avatar cache and refetch avatars when image decoding/network glitches occur.
- In parent channel rooms: create thread from message (`C-c C-t m`) or detached thread (`C-c C-t c`).
- In thread rooms: join (`C-c C-j`), leave (`C-c C-l`), toggle archived (`C-c C-t a`), rename/lock/slowmode/auto-archive/settings (`C-c C-t ...`), and set current-user mute (`C-c C-t u`).
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
- Root now supports multiple layouts: a compact account home, a telega-style
  activity list (one-line rows as `<icon> [channel | category | guild]
  <preview> <time>`), and user-defined custom layouts via layout specs.
- Activity layout excludes thread rows by default to keep large guilds responsive; enable them with `disco-root-activity-include-threads` when needed.
- Root startup is summary-first: guild and DM indexes load without a per-guild
  REST fan-out.  READY channel snapshots are reused, and opening a guild
  hydrates only that guild.  `g` refreshes the index; `C-u g` explicitly
  refreshes every guild directory.
- Guild directories use persistent keyed EWOCs, collapsible categories,
  text/unread lenses, active-thread nesting, and incremental Gateway patches.
  Missing latest-message previews are hydrated in serialized batches through
  user Gateway opcode 34 (`LAST_MESSAGES`) instead of displaying a placeholder.
- Root rendering uses EWOC plus debounced live-update aggregation; home and
  activity layouts patch rows incrementally, with activity row reordering to
  track sort changes without full buffer rebuilds.
- Room timeline rendering now uses EWOC, with local message-row refresh on live create/update/delete events.
- Request serialization and rate-limit-aware retries for Discord REST calls.
- Optional telega-style global presentation modes: enable
  `disco-client-mode-line-mode` for clickable unread/mention status and
  `disco-notifications-mode` for delayed, visibility-aware desktop alerts.

## Dependencies

- Emacs 27.1+
- `plz` (required): HTTP transport backend
- `websocket` (required): Discord Gateway websocket transport
- `appkit` (required): shared view, chat, presentation, and media runtime

## File Layout

- `disco.el`: package entrypoint.
- `disco-customize.el`: user options and token command.
- `disco-ins.el`: Discord attachment and message insertion adapter over appkit.
- `disco-media.el`: Discord attachment, spoiler, waveform, and audio adapter.
- `disco-api.el`: REST requests (sync + async callback paths).
- `disco-http.el`: HTTP wrapper on `plz` (sync + async queue-backed paths).
- `disco-permission.el`: shared Discord permission bitfield constants/parsing/check helpers.
- `disco-state.el`: in-memory guild/channel/message cache.
- `disco-directory.el`: request owner for guild/DM indexes and lazy per-guild channel snapshots.
- `disco-preview.el`: shared, rate-limit-aware lifecycle owner for channel
  latest-message preview hydration.
- `disco-gateway.el`: Discord Gateway websocket transport and dispatch hook.
- `disco-root-layout.el`: root layout registry, entry/view-spec structs, and layout composition helpers.
- `disco-root-view.el`: root-specific view state, row-model helpers, inserters,
  EWOC/list renderers, and archived-thread/root layout builders; controller
  callbacks are injected from `disco-root.el`.
- `disco-root.el`: root dashboard controllers, live updates, search commands, and buffer orchestration.
- `disco-channel-directory.el`: lazy per-guild category/channel/thread browser.
- `disco-room.el`: room buffer render/send flow with async refresh/pagination.
- `disco-company.el`: composer completion engine (`@`/`#`/`:`, CAPF, optional company backend).

## Custom Layout Example

Use `:build` when you want a custom layout to return a view spec instead of
rendering buffer contents directly. For simple custom layouts, pair `:build`
with `:update-mode full` and return a `list-spec`:

```elisp
(defun my-disco-root-build-dm-focus ()
  (let ((channels (disco-root--visible-private-channels)))
    (disco-root-layout-list-spec-view-spec-create
     (appkit-view-list-spec-create
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

If you want an EWOC-backed custom layout that reuses the same entry pipeline as
built-in home/activity layouts, return an `items` view spec instead:

```elisp
(defun my-disco-root-build-people-ewoc ()
  (disco-root-layout-ewoc-entry-view-spec-create
   (mapcar (lambda (channel)
             (disco-root-layout-entry-create
              :type 'channel
              :channel channel
              :indent 2
              :scope 'root))
           (disco-root--visible-private-channels))))
```

## Runtime Observability

- `M-x disco-describe-http-queue`: show current queue limit/active/pending counts.
- `M-x disco-describe-rate-limits`: open a buffer showing global/route/bucket cooldown state.
- `M-x disco-describe-gateway`: show websocket state, watched channel count, resume/session info, and reconnect backoff state.

## Thread Commands

- Root buffer: `A` opens archived thread browser for a selected parent channel.
- Guild directory: `RET`/`TAB` on forum/media folds active posts inline; first
  expansion lazily fetches every `/channels/{id}/threads/search`
  (`archived=false`) page. Post rows consistently preview the returned starter
  message; unhydrated posts remain under the parent loading state.
- Guild directory: `g` on a forum/media row refreshes only that parent; `A` on
  a parent or child post opens the parent's paginated archived-thread browser.
- Threads beneath ordinary text channels are opened from messages, references,
  or search results; they are not inserted into the guild channel directory.
- Archived thread buffer: `g` refreshes from first page, `n` loads next page, `RET`/mouse opens selected thread.
- Archived thread fetch only queries the `private` source when `MANAGE_THREADS` is present, and also suppresses expected permission-denied (`Missing Access`) source noise.
- Thread room buffer: `C-c C-j` join, `C-c C-l` leave, `C-c C-t a` toggle archived state.
- Thread room buffer: `C-c C-t r` rename, `C-c C-t k` toggle locked, `C-c C-t s` set slowmode, `C-c C-t A` set auto-archive duration, `C-c C-t e` edit multi-field settings, `C-c C-t u` set thread mute.
- Parent room buffer: `C-c C-t m` creates from message, `C-c C-t c` creates detached thread.
- Room transient (`?`): includes message send/refresh plus thread create/join/leave/archive/rename/lock/slowmode/auto-archive/settings/mute actions.
- Composer-aligned room keys use telega-style slots where possible: `C-c C-o` opens room input options, while `C-c C-e` formatting and `C-c C-v` clipboard attach are reserved placeholders until fuller parity lands.

## Message Commands

- Room buffer: `r` set reply target from message-at-point, `C-c C-k` clears pending reply.
- Room buffer: `C-c m f` forwards a message by id/channel, with optional comment and optional forward-only subset (`embed_indices` / `attachment_ids`) chosen from source message entries. Legacy `C-c C-F` still works. In picker prompts, press `RET` on empty input to skip one side.
- Room buffer: `e` edits message-at-point, `d` deletes message-at-point (with confirmation).
- Room history extends automatically near its older and newer visible edges using
  Discord's `before`/`after` cursor pagination; `M-<` and `M->` keep their native
  beginning/end-of-buffer bindings.
- Room buffer draft: attachment objects can be removed at point with `C-c C-d`; `M-r` searches draft history; `M-RET` opens a parsed composer preview buffer.
- Room transient (`?`): includes load older / reply / cancel reply / edit / delete actions.
- Room buffer: `c` copies at point in DWIM fashion (region/url/code/message text), `l` copies the current message permalink, `n`/`p` move across visible messages, `o` opens the message action transient, `i` describes the message, `L` redisplays it, and `C-c m c/l/n/p/o/t/i/L` expose the msg-centric copy/action family.
- Room timeline keys outside the composer follow telega-style message actions where available: `c` copy-dwim, `l` copy-link, `n` next, `p` previous, `o` message actions, `r` reply, `f` forward, `e` edit, `d` delete, `i` describe, `L` redisplay, `!` add reaction, `+` toggle reaction, `-` remove reaction, `T` open thread.
- Room poll actions: `C-c C-p s` send poll, `C-c C-p +` select answer, `C-c C-p -` unselect answer, `C-c C-p t` toggle staged answer, `C-c C-p v` submit staged vote, `C-c C-p c` remove own vote, `C-c C-p e` end poll.
- Room transient (`?`): includes attachment/forward and reaction/poll actions (`f`, `F`, `D`, `x`, `v`, `V`, `O`, `!`, `+`, `-`, `p`, `w`, `u`, `t`, `W`, `C`, `X`).
- Room transient (`?`): thread section includes create/open/manage actions (`m`, `o`, `n`, `R`, `L`, `S`, `U`, `E`, `M`, `j`, `l`, `a`, `A`).
- Mention send policy can be tuned via `disco-room-allowed-mentions` and `disco-room-reply-mention-replied-user`.
- `disco-room-enable-company-backend` controls optional company integration for composer completion (`disco-room-company-completion`); `disco-company-show-user-avatars` toggles avatar rendering, and `disco-company-capf-avatar-size` keeps completion row height stable for both Corfu/CAPF and company.
- Root channel labels show `[read]` when local read cursor reaches known channel `last_message_id`.
- Root gateway, directory, preview, search, and geometry updates accumulate
  precise Appkit invalidations and share one view-owned synchronization pass.
- `disco-root-default-layout`, `disco-root-custom-layouts`, `disco-root-tree-default-show-unread-section`, and `disco-root-tree-unread-section-limit` control root layout behavior; custom layouts are built with `:build` view-spec builders.
- `disco-root-activity-context-width` controls the left context block width in activity rows (telega-like fixed/ratio/bounded semantics).
- `disco-root-activity-include-threads` controls whether thread channels are listed in activity layout (default off for performance).
- `disco-root-activity-time-format-alist` and `disco-root-week-start-day` control telega-like activity timestamp formatting buckets.
- `disco-root-activity-time-column-width` reserves a stable right-aligned time
  slot across short weekday, full-date, and empty timestamps.
- `disco-root-auto-fill-on-window-size-change` keeps root rows auto-aligned when window width/text scale changes; `disco-root-auto-fill-margin-columns` reserves extra right margin, and `M-x disco-root-buffer-auto-fill` forces one manual reflow.
- `disco-root-extra-info-functions` lets you inject extra row metadata without blocking network calls in the renderer.
- Guild directories use `/` for a text lens, `U` for the unread lens,
  `RET`/`TAB` to open channels or toggle categories, and `g` to force-refresh the
  selected guild only.
- `disco-preview-fetch-enabled`, `disco-preview-fetch-debounce`, and
  `disco-preview-response-timeout` control opcode-34 preview hydration.

## Gateway Configuration

- `disco-gateway-version`: gateway API version (default now aligned to `v10`).
- `disco-gateway-transport-compression`: optional transport compression (`zlib-stream` or disabled).
- `disco-gateway-zlib-max-buffer-bytes`: safety cap for accumulated compressed stream bytes.
- `disco-gateway-identify-intents`: optional identify intents bitmask.
  - If intents are explicitly set, include `GUILD_MESSAGE_TYPING` (`1<<11`) and/or `DIRECT_MESSAGE_TYPING` (`1<<14`) to receive typing events.
  - Include `GUILD_MESSAGE_POLLS` (`1<<24`) and/or `DIRECT_MESSAGE_POLLS` (`1<<25`) to receive `MESSAGE_POLL_VOTE_ADD` / `MESSAGE_POLL_VOTE_REMOVE` events.
- `disco-gateway-identify-capabilities`: additional identify capabilities bitmask, merged with disco.el's required `CHANNEL_OBFUSCATION` capability and passive-v2 when enabled below.
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
- Guild directories defer passive EWOC mutations while hidden, then coalesce
  dirty channel IDs when redisplayed with real window metrics; this prevents
  mixed pixel/character alignment after entering and leaving a thread room.
- Room EWOC state keeps message-node indexes; reaction and poll-vote events patch rows locally while create/update/delete rerender fully to preserve grouping/day/unread layout correctness.
- Shared view primitives are reused by room cards, guild directories, and
  archived-thread lists to keep UI interactions and list layout consistent.
- Avatar fetch/render pipeline is asynchronous and rerenders room buffers when images become available.
- Composer completion is token-boundary aware for `@`/`#`/`:`, with dynamic candidate lists and optional company backend integration (`disco-room-company-completion`). Guild custom emoji and role snapshots are kept independently from compact guild directory objects and updated by Gateway events.
- Gateway `READY` read-state payload and `MESSAGE_ACK` dispatch update local read cursors/unread mentions.
- Gateway transport supports optional `compress=zlib-stream` and decodes binary payloads with a per-connection shared stream context.
- Thread channels are indexed by parent channel, rendered hierarchically in
  guild directories, and updated from
  `THREAD_CREATE`/`THREAD_UPDATE`/`THREAD_DELETE`/`THREAD_LIST_SYNC` events.
- Gateway thread membership deltas (`THREAD_MEMBER_UPDATE`/`THREAD_MEMBERS_UPDATE`) now update lightweight per-thread member caches.
- Gateway reconnect uses exponential backoff with jitter for transport failures, and randomized delay handling for `INVALID_SESSION`.
- Identify payload supports optional intents/capabilities/presence fields through customization.
- Rate-limit handling currently surfaces 429 with retry metadata to the user; full bucket scheduler is planned next.

## Next Milestones

1. Improve mention/composer parity (mention candidate popup UX, optional multiline compose mode, fuller attach/options parity for `C-c C-e`/`C-c C-v`).
2. Expand fast navigation (`M-g` prefix map for unread/mentions/reactions style jumps).
3. Add queue prioritization/backpressure so user actions are favored over background work.
