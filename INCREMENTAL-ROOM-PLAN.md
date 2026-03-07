# disco-room incremental update plan

## Goal

Move `disco-room` closer to telega-style chatbuf behavior:

- keep composer/input undo-friendly
- avoid full room rerenders for routine live updates
- keep one persistent EWOC for the room timeline
- update header, footer, prompt, and message nodes independently
- make `emacs-qq` reuse the same infrastructure

## What telega is doing differently

Observed from `telega-chat.el` and `telega-core.el`:

- the chat buffer keeps a stable input marker
- prompt/footer updates are incremental (`telega-chatbuf--footer-update`,
  `telega-chatbuf--prompt-update`)
- message area lives in a persistent EWOC
- ordinary typing is not globally undo-disabled
- special bulk UI mutations may locally avoid undo noise, but input itself keeps
  normal editing semantics

## Current disco-room state

Already present:

- EWOC timeline
- message node table
- partial node helpers for insert/update/delete
- shared chat input module: `disco-chat-input.el`

Still blocking telega-like behavior:

- `disco-room-render` recreates the EWOC and message-node table
- header/footer/composer are rebuilt together with the whole room
- message create/update/delete still falls back to full rerender because layout
  context depends on neighboring messages
- async avatar/preview/download updates often rerender more than necessary

## Target architecture

### 1. Stable room frame

Keep these objects stable for the entire room buffer lifetime:

- `disco-room--ewoc`
- `disco-room--message-node-table`
- `disco-room--render-context-by-message-id`
- composer markers and footer properties

Only recreate them on hard resets:

- opening a different room in the buffer
- layout/filter mode changes that invalidate all rendered nodes
- explicit full reset paths

### 2. Split rendering responsibilities

Break current `disco-room-render` into explicit update layers:

- `disco-room--render-header`
- `disco-room--render-footer`
- `disco-room--render-composer`
- `disco-room--render-full-timeline`
- `disco-room--render-full`

This makes it possible to refresh only the dirty surface.

### 3. Localized timeline invalidation

Introduce neighborhood-aware invalidation helpers:

- `disco-room--message-prev-id`
- `disco-room--message-next-id`
- `disco-room--invalidate-message-neighborhood`
- `disco-room--recompute-render-context-around`

Reason: grouping, date separators, and unread divider depend on nearby messages,
not only on the current node.

### 4. Dirty-region model

Track which parts of the room must update:

- header dirty
- footer dirty
- composer dirty
- timeline full dirty
- message-id set dirty

Then one dispatcher decides whether to do:

- footer-only refresh
- prompt/composer-only refresh
- node invalidation for a small message neighborhood
- full timeline refresh

## Phases

### Phase 0 - done

- extract shared composer/input logic to `disco-chat-input.el`
- switch `disco-room` and `emacs-qq` chat input to the shared module

### Phase 1 - persistent composer/footer

1. keep one EWOC instance alive after room creation
2. replace EWOC footer text in place instead of recreating EWOC on every render
3. make composer updates preserve point/window/input offsets
4. ensure chat input undo stays intact during footer refreshes

Expected win:

- telega-like input behavior
- much less undo pollution
- less point jitter while typing

### Phase 2 - persistent timeline container

1. stop recreating `disco-room--message-node-table` on ordinary render
2. add explicit hard-reset path for true full rebuilds
3. create helpers to reconcile displayed message ids with current state
4. reuse existing nodes whenever message identity remains stable

Expected win:

- avatars/previews/reactions can update existing nodes
- less allocation and less flicker

### Phase 3 - neighborhood incremental updates

1. compute render context for one message from `(prev current next)`
2. invalidate current node plus minimal affected neighbors
3. use partial path for:
   - message create
   - message update
   - message delete
   - recall/edit-like events
4. keep full rerender as fallback only when neighborhood reasoning fails

Expected win:

- new messages no longer force full room redraw
- compact grouping and date separators remain correct

### Phase 4 - async media/UI patching

Convert these to node-local updates:

- avatar fetch completion
- attachment preview completion
- download-state changes
- spoiler reveal reset
- reaction and poll changes

Expected win:

- async resource completion no longer redraws the whole room

### Phase 5 - history paging and jumps

1. prepend older messages into EWOC without full rebuild
2. append newer history when needed without full rebuild
3. preserve window anchor and input offsets during page merges
4. keep unread divider logic correct after incremental history insertion

Expected win:

- scrolling near room edges behaves more like telega

### Phase 6 - expose reusable chat timeline API

Once `disco-room` stabilizes, extract generic pieces for reuse by other clients:

- persistent timeline container
- node reconciliation helpers
- neighborhood invalidation helpers
- composer/footer updater hooks

This is the point where `emacs-qq` can reuse not only the input layer, but also
more of the timeline engine directly.

## Immediate implementation order

1. Phase 1 first
2. then Phase 2
3. then Phase 3
4. only after that optimize media patch paths further

Reason:

- stable composer/footer is the biggest user-facing pain
- stable EWOC lifetime is required before local message invalidation becomes
  worth doing
- async media optimizations are much safer once the container is persistent

## Risk areas

- compact grouping is neighbor-sensitive
- date separators are neighbor-sensitive
- unread divider is globally sensitive to last-read state
- filter mode may still require full timeline rebuilds
- width changes and text scaling may still require full visible rerender

## Success criteria

We can consider the refactor successful when all are true:

- typing in `disco-room` keeps normal undo history
- footer/prompt updates do not recreate EWOC
- message create/update/delete usually invalidate only local nodes
- avatar/preview completion usually invalidates only local nodes
- `emacs-qq` can consume the same shared infrastructure with little or no copy
