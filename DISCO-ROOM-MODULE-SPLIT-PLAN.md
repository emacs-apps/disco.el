# disco-room module split plan

## Purpose

This document turns the high-level chatbuf alignment plan into a concrete file
split plan for `disco-room`.

It assumes these goals are already accepted:

- `disco-room` should move closer to telega's chatbuf architecture
- keybinding layout should track telega's binding clusters where features exist
- the long-term composer model is shared chatbuf core + room adapter, not one
  giant room file

This plan is about module boundaries, migration order, ownership, and test
strategy.

## Why split now

`disco.el/disco-room.el` is currently a single file with several different jobs:

- room mode and keymaps
- composer state, draft parsing, attachments, send pipeline
- message-at-point actions
- search/filter/jump flows
- thread management UI
- render helpers and EWOC reconcile
- avatar/media/forward card insertion helpers
- gateway event handling and partial patching
- transient menus
- optimistic read state bookkeeping

That is much closer to a bundle of `telega-chat.el` + `telega-msg.el` +
`telega-ins.el` + `telega-transient.el` + parts of `telega-tdlib-events.el`
than to a single coherent module.

The problem is not just size.  It is coupling:

- composer work risks breaking render/event code
- keybinding cleanup risks touching unrelated thread/search/render code
- partial patch logic is hard to audit because render ownership is diffuse
- tests are forced into one integration-heavy file instead of matching
  ownership boundaries

## Desired end state

After the split, `disco-room.el` should be a facade/adaptor, not the home of
all room logic.

It should mostly own:

- `disco-room-mode`
- room-local vars that define public state shape
- the top-level keymap assembly
- `disco-room-open` and buffer lifecycle setup/teardown
- thin forwarding wrappers where compatibility is needed
- requires/provides wiring for the room subsystem

Everything else should live in smaller files with tighter ownership.

Target outcome:

- `disco-room.el` becomes the adapter layer for room buffers
- composer code is isolated from render/event code
- message-local actions are isolated from composer and thread settings
- render and event patch logic are separately reviewable
- tests are moved to module-level suites with one smaller room integration suite

## Design principles

- Split by ownership, not by arbitrary line count.
- Prefer telega-like ownership boundaries over Discord endpoint groupings.
- Keep migration steps behavior-preserving whenever possible.
- Add forwarding wrappers before deleting old definitions.
- Move low-risk leaves before high-risk reconcile logic.
- Keep docs/help text synchronized with the binding cluster they describe.
- Avoid introducing new cycles between room modules.

## Telega correspondence

The goal is not file-name mimicry.  The goal is to copy telega's responsibility
layout where it makes sense for Discord.

| disco target | Main responsibility | Telega reference | Notes |
| --- | --- | --- | --- |
| `disco-room.el` | room facade/adaptor | `telega-chat.el` chatbuf entrypoints | thin shell only |
| `disco-room-compose.el` | composer, draft, attach, send, input options | `telega-chat.el` chatbuf input/attach/send | highest priority |
| `disco-room-message.el` | message-at-point actions and local key semantics | `telega-msg.el` + message commands in `telega-chat.el` | reply/edit/forward/delete/reactions |
| `disco-room-render.el` | message insertion and header/footer composition | `telega-ins.el` + render parts of `telega-chat.el` | may later split again |
| `disco-room-events.el` | gateway-driven updates and local patch application | `telega-tdlib-events.el` + `telega-chat.el` dirtiness/update code | keep partial patching here |
| `disco-room-search.el` | filter search, inplace search, jump flows, draft-history search | search/filter sections of `telega-chat.el` | message search is its own concern |
| `disco-room-thread-ui.el` | room-level thread commands and prompts | thread/topic command areas in `telega-chat.el` | should build on `disco-thread.el` |
| `disco-room-transient.el` | room transients and command menus | `telega-transient.el` | low-risk extraction |
| `disco-room-avatar.el` (optional) | avatar cache/fetch/render helpers | avatar/media helpers spread across telega | only if render remains too large |
| `disco-room-read-state.el` (optional) | optimistic ack/read bookkeeping | read-state update helpers in telega chat/events | optional follow-up |

## Proposed module boundaries

### 1. `disco-room.el`

Owns:

- room mode definition and setup/teardown
- room-local state variable declarations
- top-level keymap assembly
- requires/provides for room subsystem
- high-level public entry points that other modules call back into:
  - `disco-room-open`
  - `disco-room-refresh`
  - `disco-room-render`
  - facade wrappers kept for compatibility

Should not own long implementations for:

- send paths
- render section insertion
- gateway event dispatch
- search/filter algorithms
- thread setting prompts
- transient definitions

### 2. `disco-room-compose.el`

Owns all composer and send-pipeline logic.

Move here first:

- composer availability and permission helpers directly tied to input/send
- aux-state entry/exit helpers:
  - `disco-room--composer-*`
  - `disco-room-cancel-reply`
  - `disco-room-return-dwim`
- draft/input helpers:
  - `disco-room--copy-draft`
  - `disco-room--current-draft*`
  - `disco-room--input-*`
  - `disco-room--sync-draft-from-buffer`
  - `disco-room-draft-prev`
  - `disco-room-draft-next`
  - `disco-room-edit-draft`
  - `disco-room-draft-history-search`
- structured input object and attachment helpers:
  - `disco-room--attachment-input-object-*`
  - `disco-room--attachment-refs`
  - `disco-room--parse-draft-input`
  - `disco-room-list-attachments`
  - `disco-room-edit-attachment-description`
  - `disco-room-reorder-attachments`
  - `disco-room-remove-attachment-token-at-point`
  - `disco-room-clear-attachments`
  - `disco-room-attach-file`
  - `disco-room-attach-clipboard`
  - `disco-room-attach-transient`
- input option state and option commands:
  - `disco-room--current-input-options-state`
  - `disco-room--sync-shared-input-options-state`
  - `disco-room-toggle-send-on-return`
  - `disco-room-cycle-long-message-action`
  - `disco-room-cycle-allowed-mentions`
  - `disco-room-toggle-reply-mention-replied-user`
  - `disco-room-reset-input-options`
  - `disco-room-input-options-transient`
  - `disco-room-input-formatting-set`
  - `disco-room-input-preview`
- send pipeline:
  - `disco-room--send-allowed-mentions`
  - long-message split/file helpers
  - `disco-room-send-message`
  - `disco-room-send-poll`

Dependency rule:

- may depend on `disco-chatbuf.el`, `disco-api.el`, `disco-thread.el`,
  `disco-msg.el`, `disco-permission.el`
- must not depend on low-level render internals
- should talk to rendering through facade calls only, such as
  `disco-room--update-frame-preserving-point` and `disco-room-refresh`

### 3. `disco-room-message.el`

Owns message-at-point actions and message-local interaction semantics.

Move here:

- message-id/message-at-point helpers used by actions
- reply/edit/forward/delete commands:
  - `disco-room-reply-to-message`
  - `disco-room-forward-message`
  - `disco-room-edit-message`
  - `disco-room-delete-message`
- forward-only prompt helpers
- reaction commands and helpers:
  - `disco-room-add-reaction`
  - `disco-room-remove-reaction`
  - `disco-room-toggle-reaction`
  - reaction input parsing helpers
- message prefix keymap / message-local bare keys
- open-thread-from-message command
- poll vote/submit/clear/expire commands if we keep message interaction grouped

Telega correspondence:

- this should become the closest disco equivalent to `telega-msg.el`
- bare timeline keys should be documented here as the primary message-local
  layer, with `C-c m ...` as prefix fallback

### 4. `disco-room-search.el`

Owns room search and filtering.

Move here:

- message text extraction used only for searching
- filter search state helpers and API dispatch
- inplace search state and movement helpers
- sender search candidate helpers
- jump-to-message flow and fetch-around logic
- search UI commands:
  - `disco-room-filter-search`
  - `disco-room-filter-refresh`
  - `disco-room-filter-load-more`
  - `disco-room-filter-cancel`
  - `disco-room-inplace-search*`
  - `disco-room-search*`
  - `disco-room-jump-to-message`
  - `disco-room-search-channel`

Dependency rule:

- may depend on state/api/msg helpers and room facade render hooks
- should not own message render context or gateway event logic

### 5. `disco-room-thread-ui.el`

Owns room commands that manage Discord thread settings and lifecycle.

Move here:

- thread availability checks that are room-UI specific
- thread prompts not already generic in `disco-thread.el`
- commands:
  - `disco-room-create-thread-from-message`
  - `disco-room-create-thread`
  - `disco-room-join-thread`
  - `disco-room-leave-thread`
  - `disco-room-toggle-thread-archived`
  - `disco-room-rename-thread`
  - `disco-room-toggle-thread-locked`
  - `disco-room-set-thread-slowmode`
  - `disco-room-set-thread-auto-archive-duration`
  - `disco-room-set-thread-muted`
  - `disco-room-edit-thread-settings`
  - `disco-room-open-parent-archived-threads`

At the same time, expand `disco-thread.el` only for helpers that are genuinely
shared between root/room/other UIs.

### 6. `disco-room-render.el`

Owns the room's textual/visual rendering and EWOC reconcile.

Move here in two sublayers.

Render leaf layer first:

- layout/fill/alignment helpers
- divider and prefix insertion helpers
- author/avatar/forward/media/poll/reaction insertion helpers
- `disco-room--insert-message-*` family
- header/footer/prompt text composition
- message preview rendering helpers

Then render core:

- EWOC create/update/clear helpers
- render-context computation and invalidation
- timeline reconcile / node upsert/delete
- `disco-room-render`
- `disco-room--render-preserving-point`
- `disco-room--update-frame-preserving-point`

Preferred internal split if needed:

- keep `disco-room-render.el` as orchestrator
- extract avatar-heavy cache/fetch code later into `disco-room-avatar.el`
  only if render remains too large after the first pass

Telega correspondence:

- this is the nearest disco equivalent to `telega-ins.el`
- high churn risk; do this after compose/search/transient split patterns are
  established

### 7. `disco-room-events.el`

Owns event-driven updates and local partial patch logic.

Move here:

- optimistic read/ack state helpers
- local message update helpers
- partial patch helpers:
  - live message/reaction/poll/read-state patching
  - forward-source dependency invalidation
  - composer-context invalidation decisions
- gateway attachment and handler lifecycle:
  - `disco-room--handle-gateway-event`
  - `disco-room--attach-live-updates`
  - `disco-room--detach-live-updates`
- around-fetch/jump resolve if left coupled to event/update state

Telega correspondence:

- nearest disco equivalent to `telega-tdlib-events.el`
- keep this module centered on incremental updates, not on initial full render

### 8. `disco-room-transient.el`

Owns transient definitions only.

Move here:

- `disco-room-transient`
- any future room attach/message/thread transients that grow further

This is intentionally small and low risk.  It is a good first extraction to set
module conventions.

## Existing shared modules to expand instead of adding room-specific files

### `disco-chatbuf.el`

Continue growing this for shared chatbuf mechanics only:

- prompt lifecycle
- input markers
- input history ring primitives
- aux/input-options state storage
- structured input object primitives

Do not move Discord-specific send rules or room permission logic here.

### `disco-msg.el`

Expand this for shared message model helpers, not room UI commands.

Good candidates:

- more message identity/reference helpers
- normalized author/title/preview helpers
- reusable forward/reference predicates

Do not move room-specific interactive commands here until a broader message UI
layer exists.

### `disco-thread.el`

Expand this for shared thread predicates and declarative update helpers.

Good candidates:

- thread status derivation
- shared prompt choice helpers
- update application helpers

Keep room command flows in `disco-room-thread-ui.el`.

### `disco-media.el` / `disco-embed.el`

If some attachment/embed insertion logic becomes generic enough, move it into
these modules rather than inventing `disco-room-attachment-render.el` too early.

## Recommended migration order

### Milestone 0 - establish facade and conventions

- create empty target files with `provide`
- require them from `disco-room.el`
- define one naming rule:
  - room facade keeps `disco-room-*`
  - internal module helpers may still use `disco-room--*`
  - file ownership is documented in file header commentary
- add one brief ownership note to each new file header

Acceptance:

- no behavior change
- load path and requires are stable

### Milestone 1 - extract transients and search

Why first:

- lowest coupling
- easiest way to validate module style
- least likely to break composer/render

Move:

- `disco-room-transient`
- `disco-room-search*`, filter/inplace helpers, jump helpers

Acceptance:

- search behavior unchanged
- room transient unchanged
- tests split into `test/disco-room-search-test.el` and a transient smoke test

### Milestone 2 - extract composer core

Move:

- aux bridge
- draft/input helpers
- structured input object helpers
- attachment manipulation UI
- history search / preview / input options

Acceptance:

- all current rich draft and attachment object tests pass
- no loss of text properties across redraw/send failure/edit cancel
- keybinding docs still match actual bindings

### Milestone 3 - extract send pipeline

Move:

- send allowed_mentions
- long-message helpers
- send-message / send-poll
- attach commands and attach transient

Acceptance:

- send tests remain green
- edit restore state still works
- object-based attachment send path remains the only advertised path

### Milestone 4 - extract message-at-point actions

Move:

- reply/edit/forward/delete
- reactions
- poll voting actions if grouped here
- message-local keymap helpers

Acceptance:

- bare timeline keys still work only outside composer
- message prefix fallback still works
- forward flow remains compatible with current Discord API behavior

### Milestone 5 - extract thread UI commands

Move:

- thread create/manage/join/leave/archive/settings commands
- parent archived thread opener

Acceptance:

- thread tests move with the code
- no room composer regressions

### Milestone 6 - extract render leaf helpers

Move first:

- layout/prefix/alignment helpers
- author/avatar/media/embed/forward/reply section insertion helpers
- header/footer/prompt help text

Acceptance:

- byte-for-byte visible output should stay as close as practical
- render-specific tests remain green
- no event logic moved yet

### Milestone 7 - extract render core and event patchers

Move:

- EWOC lifecycle
- render-context recomputation
- node reconcile
- local partial patch helpers
- gateway event handler wiring
- optimistic read/ack helpers if still coupled

Acceptance:

- integration tests for live update patching remain green
- full rerender fallback still works when partial patch cannot apply

### Milestone 8 - shrink `disco-room.el` to facade

After all major moves:

- remove long implementations from `disco-room.el`
- keep only facade wrappers where external call sites still expect old symbol
  locations
- review whether some wrappers can be dropped after one compatibility cycle

Success criterion:

- `disco-room.el` is primarily mode wiring + public adapter layer
- module ownership is obvious from file names and headers

## Dependency rules

To prevent a new spaghetti graph, use these rules.

- `disco-room.el` may require every room submodule.
- `disco-room-compose.el` may call facade render hooks but must not require
  low-level render internals.
- `disco-room-render.el` should not depend on composer parsing internals.
- `disco-room-events.el` may depend on render facade and message helpers, but
  should not own attach/send logic.
- `disco-room-search.el` may depend on state/api/facade render hooks, but not
  on render section insertion helpers.
- `disco-room-transient.el` should depend on public commands only.

If a helper is needed by both compose and render, prefer moving it into a shared
module (`disco-chatbuf.el`, `disco-msg.el`, `disco-thread.el`) instead of
creating circular room-module dependencies.

## Test split plan

Current tests are concentrated in `test/disco-room-test.el`.  After extraction,
create focused test files while keeping a smaller room integration suite.

Recommended files:

- `test/disco-room-compose-test.el`
- `test/disco-room-search-test.el`
- `test/disco-room-message-test.el`
- `test/disco-room-thread-ui-test.el`
- `test/disco-room-events-test.el`
- `test/disco-room-render-test.el`
- `test/disco-room-test.el` kept as cross-module smoke/integration suite

Rule:

- move tests with the code at the same milestone the code moves
- only keep end-to-end interaction tests in `test/disco-room-test.el`

## Risk register

### 1. Cyclic dependencies

Most likely between compose, render, and events.

Mitigation:

- keep room facade as the only cross-module rendezvous when possible
- move shared pure helpers into `disco-chatbuf.el`, `disco-msg.el`, or
  `disco-thread.el`

### 2. Draft property loss

Composer extraction can accidentally strip text properties or object boundaries.

Mitigation:

- add explicit tests for property-preserving draft copies
- keep send failure/edit cancel coverage in compose tests

### 3. Partial patch regressions

Render/event split can silently break local invalidation.

Mitigation:

- do not split event patchers before render ownership is clear
- keep integration tests for gateway create/update/delete/reaction/poll flows

### 4. Keybinding drift

New module layout can desynchronize docs/help text/keymaps.

Mitigation:

- keep keymap comments in the owning module
- update README/help text in the same patch as binding changes
- add binding assertions to tests for the primary telega-aligned slots

### 5. Over-fragmentation

Too many tiny files can make room logic harder to follow.

Mitigation:

- target files in the rough range of 500-1500 lines where practical
- only create optional modules like `disco-room-avatar.el` when there is a real
  ownership seam, not just because a section is long

## What not to do

- do not split one file per command if they still share one state machine
- do not move Discord-specific send logic into `disco-chatbuf.el`
- do not make `disco-room-render.el` call deep composer internals directly
- do not let transient files become the place where real business logic lives
- do not remove compatibility wrappers until docs/tests and call sites are all
  updated

## Recommended first concrete patch series

If work starts immediately, the lowest-risk sequence is:

1. add this plan reference from `INCREMENTAL-ROOM-PLAN.md`
2. create `disco-room-transient.el` and move transient definitions
3. create `disco-room-search.el` and move filter/inplace/jump logic
4. create `disco-room-compose.el` and move draft/object/preview/options helpers
5. move send pipeline into the same compose module
6. create `disco-room-message.el` and move reply/forward/edit/delete/reaction
   actions

That gets the file split pattern established before touching the most fragile
render/event internals.

## Exit criteria

This split plan is considered complete when:

- `disco-room.el` is mostly facade code
- major responsibilities each have one obvious home
- keybinding clusters are documented by module ownership
- tests are split by ownership with one smaller integration suite
- future telega-parity work can happen inside the right module without opening a
  9000-line file every time
