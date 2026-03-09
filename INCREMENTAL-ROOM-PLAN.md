# disco-room telega chatbuf alignment plan

## Status

This plan describes the behavioral and sequencing side of the `disco-room`
refactor.

It complements these documents:

- `emacs-qq/TELEGA-CHATBUF-ALIGNMENT-DESIGN.md`
- `emacs-qq/INCREMENTAL-CHAT-PLAN.md`
- `DISCO-ROOM-MODULE-SPLIT-PLAN.md`

The split plan answers "who should own this code?".

This file answers:

- what telega-aligned chatbuf behavior we still want
- what has already landed
- what the remaining architectural gaps are
- what order the next migrations should happen in

The main remaining gap is no longer basic timeline persistence.

The main remaining gaps are:

- completing the move toward a real tail-input chatbuf model
- moving shared behavior out of `disco-room.el` into its real owners
- isolating truly room-specific render and event logic from shared helpers

## Goal

Move `disco-room` toward the same telega-style chatbuf model now targeted for
both clients, while following an ownership-first architecture.

The intended end state is:

- one real tail input region in the buffer
- prompt and footer updated independently from input contents
- reply/edit/forward-like state represented as aux state
- structured input objects instead of text-only draft conventions
- normal typing and undo behavior
- printable single-key room actions available contextually on the timeline,
  without stealing ordinary typing from the input area
- a shared chatbuf core in `disco.el` that `disco-room` and `qq-chat` both use
- `disco-room.el` reduced to a facade/public surface rather than a giant owner
  of unrelated internals

This is not a demand to clone telega's exact visuals.  It is a demand to follow
telega's chatbuf behavior and responsibility layout much more closely than the
older disco footer-composer design.

## What already changed

Compared with the older room plan, the following pieces are already in place.

### Composer and draft groundwork

- rich draft copying now preserves text properties needed for structured input
  objects
- attachment input objects exist and are inserted as structured draft objects
  instead of being created only as visible `[file:n]` text
- current send flow parses draft input into content plus attachment payloads
- attachment operations work on structured objects and still tolerate legacy
  token drafts for compatibility

### Keybinding convergence already started

- composer and attachment slots now follow telega-like binding clusters more
  closely
- message-at-point bare timeline keys are available when point is outside the
  composer
- room-local input options and preview/history commands now have telega-like
  entry points
- `disco-root` review is started conceptually, though not complete yet

### Structural extraction already started

- `disco-room-search.el` now owns room search/filter/inplace search as a first
  decoupling step
- the important lesson from that extraction is decoupling, not that every new
  module should be named `disco-room-*`

## Ownership rule

This plan now follows the ownership-first rule from
`DISCO-ROOM-MODULE-SPLIT-PLAN.md`.

That means:

- `disco-room` is a facade and public UI surface
- `disco-room` is not the default namespace for new internals
- shared chatbuf mechanics should move into `disco-chatbuf.el`
- shared message/domain helpers should move into `disco-msg.el`
- shared thread helpers should move into `disco-thread.el`
- media/embed logic should move into `disco-media.el` or `disco-embed.el` when
  it is not truly room-specific
- only irreducibly room-specific logic should stay room-owned

Practically, this means the next step is not "make more `disco-room-*` files".

The next step is:

- grow the shared owners first
- isolate room-specific render/event leftovers second
- keep public `disco-room-*` commands stable while internals relocate

## What telega is doing differently

The important telega properties to align with are behavioral and structural.

Behaviorally:

- the input region is the real tail of the chat buffer
- prompt and footer are separate update surfaces
- reply/edit live in a dedicated aux state object
- input can contain structured objects via text properties
- send logic parses the input region into backend-native payloads
- prompt/footer changes do not require rebuilding the editable input region

Structurally:

- shared chat behavior has one owner
- message behavior has one owner
- insert/render leaf helpers should have an `ins`-style owner, analogous to
  `telega-ins.el`
- room timeline orchestration does not need its own `disco-room-render` module;
  it can stay in the room facade while leaf helpers move out
- root/list UI has one owner
- event/update plumbing has one owner
- transient is an implementation detail of those owners, not a top-level split
  axis by itself

### Keybinding alignment rules

For `disco-room`, telega alignment should also apply to keybinding layout, not
just buffer architecture.

Rules:

- copy telega's binding clusters before inventing new top-level shortcuts
- distinguish telega's three chat interaction layers and mirror them where
  possible:
  - chatbuf-global keys for composer/filter/navigation
  - message-local keys for point-on-message actions
  - transient menus for richer multi-step operations
- keep chatbuf-core keys aligned where the feature exists: `RET`, `M-RET`,
  `M-p`, `M-n`, `M-r`, `C-c C-k`
- keep attach/options keys aligned where possible: `C-c C-a`, `C-c C-f`,
  `C-c C-v`, `C-c C-e`, `C-c C-o`
- keep filtering/navigation keys aligned where possible: `C-c /`, `C-c C-c`,
  `C-c C-r`, `C-c C-s`, `M-g ...`
- keep message-at-point actions aligned with telega's button-map style where
  features exist: prefer bare `r`, `f`, `e`, `d`, `!`, `+`, `-`, `T` when
  point is outside the composer, with `C-c m ...` as a compatibility/prefix
  layer rather than the primary design
- for read-only list buffers such as `disco-root`, prefer telega-style
  single-key navigation/action design (`n`/`p`/`TAB`, `RET`, `q`, `?`, `v`,
  `M-g`, `/`, `\`) over `C-c`-heavy command sets
- if `disco-room` does not yet implement telega's corresponding feature,
  prefer reserving the key over assigning an unrelated command to it
- legacy compatibility bindings may remain temporarily, but help text and docs
  should advertise the telega-aligned key first
- documentation should be organized by the same binding clusters telega uses,
  so keymap comments, help text, and README sections stay synchronized

Current high-value follow-ups:

- `C-c C-a` is the primary attachment entry point and should stay the
  telega-aligned attach anchor
- `C-c C-f` should continue evolving toward telega-like media/file attach
  semantics
- message-at-point bare keys should remain the preferred timeline interaction
  layer
- `C-c C-v` should remain reserved for clipboard/media attach semantics until
  room has a real implementation behind it
- `C-c C-e` should remain reserved until room has a real formatting operation
  behind it
- `C-c C-c` still needs an explicit long-term decision: either converge toward
  telega's filter-cancel usage or remain a documented divergence because room
  treats it as a send alias
- `disco-root` and other read-only overview buffers still need a full audit
  against `telega-root-mode-map`

## Current disco-room state

### Already aligned enough to build on

`disco-room` already has substantial groundwork:

- persistent EWOC timeline
- persistent message-node table
- render-context tracking keyed by message id
- partial patching for ordinary message changes
- point-preserving frame and timeline updates
- local invalidation for several media and message dependency cases
- initial shared chatbuf skeleton in `disco-chatbuf.el`
- structured attachment input objects in the composer path
- parsed send pipeline for content plus attachments

### Still misaligned with the target

The remaining misalignment is concentrated in ownership and chatbuf structure:

- editable input still has old footer-composer legacy mixed into the design
- prompt, footer, and input are not yet fully separated as stable surfaces
- reply/edit state still has room-local duplication instead of clean shared aux
  ownership
- some input behavior still lives in `disco-room.el` even when it really wants
  to be `disco-chatbuf` behavior
- pure message and thread helpers are still mixed into room code too often
- render and event ownership are still too concentrated in one large file
- clipboard attach and explicit formatting are still placeholders
- edit-message attachment semantics remain intentionally unsupported for now

## Target architecture

### 1. Shared owners first

The default destination for newly isolated behavior is not a new room module.

The default destinations are existing owners, plus one telega-like insert owner
when needed:

- `disco-chatbuf.el` for shared chat-buffer mechanics
- `disco-msg.el` for shared message/domain helpers
- `disco-thread.el` for shared thread helpers
- `disco-ins.el` for shared insert/render leaf helpers
- `disco-media.el` / `disco-embed.el` for reusable content rendering helpers

### 2. `disco-room` becomes a facade

After migration, `disco-room` should primarily provide:

- room-specific permission and capability rules
- room-specific public commands and keymap assembly
- room-specific EWOC/timeline orchestration
- coordination between send/render/events owners
- compatibility wrappers where needed

Prompt/footer leaf rendering and message section insertion should move below it
into `disco-ins.el` and other shared content owners.

### 3. Real tail input region

`disco-room` should stop treating the editable draft as synthetic footer text.

Instead:

- the footer stays read-only UI
- the prompt is inserted and updated independently
- the editable input begins at a stable marker after the prompt
- current draft contents are the real buffer text from input marker to point max

This remains the key behavioral shift for telega alignment.

### 4. Aux state becomes first-class

The current mix of pending reply/edit variables and room-local footer text
should converge on shared aux ownership.

The shared aux object should cover at least:

- aux type
- primary message/source object
- optional aux metadata
- saved draft state where needed

`disco-room` may keep compatibility wrappers while migrating, but send, cancel,
and display should move toward aux-driven behavior.

### 5. Structured input objects become the canonical compose model

This work is partly landed already, but the rule should now be explicit.

The canonical compose model is structured input objects carried in the draft.
Legacy visible token handling exists only as compatibility.

Likely object kinds over time include:

- attachment
- future forward/comment helper objects where useful
- future poll/send option helper objects where useful

### 6. Insert/render leaves move out, room orchestration stays thin

After shared owners absorb what belongs to them, render ownership should split
in telega-like fashion.

That mainly means:

- `disco-ins.el` owns divider/header/footer leaf builders and message section
  insertion helpers
- `disco-room.el` keeps room EWOC/timeline orchestration rather than growing a
  separate `disco-room-render.el`
- room live update and invalidation ownership is isolated separately

## Migration phases

### Phase 0 - stabilize what already landed

This phase is about keeping recent composer and keybinding gains stable.

Deliverables:

- keep structured attachment object behavior working end-to-end
- keep current parsed send behavior green in tests
- keep telega-aligned key clusters documented and tested
- keep legacy attachment token compatibility only as a temporary bridge

### Phase 1 - keep growing the shared chatbuf owner

Deliverables:

- move more input-region ownership into `disco-chatbuf.el`
- move more history/aux/input-option mechanics into `disco-chatbuf.el`
- shrink room-owned input plumbing where the behavior is not actually room-
  specific
- keep the API shape compatible enough for `qq-chat` and `disco-room`

Expected win:

- room code reads more like adapter code
- shared chatbuf invariants become easier to test in one place

### Phase 2 - move pure message and thread helpers to their shared owners

Deliverables:

- move reusable message semantics into `disco-msg.el`
- move reusable thread prompts and update helpers into `disco-thread.el`
- leave only room-specific orchestration in `disco-room.el`

Expected win:

- room commands become thinner
- shared behavior stops being hidden in room code

### Phase 3 - finish the real tail-input migration

Deliverables:

- `disco-room` stops depending on old footer-composed editing behavior
- prompt lifecycle is fully shared-chatbuf-driven
- footer becomes read-only UI only
- room draft reads and writes go through the real tail input region

Expected win:

- simpler point handling
- fewer footer hacks
- closer telega alignment for normal typing behavior

### Phase 4 - move reply/edit/forward-like behavior fully onto shared aux

Deliverables:

- reply and edit state map cleanly onto shared aux state
- cancel paths use common aux reset behavior
- prompt/footer display is derived from aux state instead of ad hoc room-local
  state
- room-local compatibility wrappers remain only where necessary

Expected win:

- cleaner command model
- easier parity with `qq-chat`
- easier future extension for richer compose modes

### Phase 5 - introduce an `ins`-style render owner

Deliverables:

- shared divider/header/footer leaf helpers move into `disco-ins.el`
- message section insertion helpers move into `disco-ins.el` or other shared
  content owners where appropriate
- EWOC lifecycle and render-context recomputation stay in `disco-room.el`
  instead of spawning a `disco-room-render.el`
- reusable media/embed bits are pushed downward where they are not truly room-
  specific

Expected win:

- render leaf changes stop colliding with composer/message/helper work
- room orchestration stays visible without becoming the default home for every
  render helper

### Phase 6 - isolate room-specific event/update ownership

Deliverables:

- gateway event hookup and partial patching are isolated from unrelated room code
- room-local invalidation policy becomes reviewable on its own
- optimistic room read/ack flows live with the update owner when still buffer-
  coupled

Expected win:

- event changes stop colliding with render and composer work

### Phase 7 - cleanup and consolidation

Deliverables:

- old `disco-chat-input.el` paths become compatibility shims or are removed
- prompt/footer update paths are clearly separate from timeline updates
- stale room-owned helper families are reviewed against their real owners again
- transitional files such as `disco-room-search.el` are reevaluated once shared
  owners stabilize

Expected win:

- one obvious place to evolve shared behavior in the future
- room-prefixed files remain only where room ownership is real

## Immediate implementation order

1. keep growing and testing `disco-chatbuf.el`
2. move more pure helpers out of `disco-room.el` into `disco-msg.el` and
   `disco-thread.el`
3. finish migrating `disco-room` from footer-composed input to true tail input
4. introduce an `ins`-style render owner while keeping EWOC/reconcile in
   `disco-room.el`
5. isolate room-specific event/update ownership
6. implement real clipboard attach and explicit formatting behind the reserved
   telega-aligned keys
7. review `disco-root` and related read-only buffers against
   `telega-root-mode-map`
8. only then remove old compatibility layers that are no longer needed

Reason:

- shared ownership clarity matters more than creating more room-prefixed files
- tail input migration is still the key behavioral shift
- render/event extraction is safer after shared owners absorb what belongs to
  them
- token compatibility removal is safest after the new compose model is fully
  settled

## Risk areas

- temporarily coexisting footer-composed and tail-input paths can still make
  point preservation tricky during migration
- attachment editing and message editing have different backend constraints and
  should not be conflated
- an extraction can reduce file size while preserving the wrong owner; that is
  still architectural debt
- transitional files can become permanent by inertia if we stop reevaluating
  their true owner
- width changes and some structural view changes may still require full visible
  rerender even after chatbuf migration

## Success criteria

We can consider this plan successful when all are true:

- typing in `disco-room` no longer depends on footer-property rebound input
- prompt and footer update independently from the editable input region
- reply/edit behavior is aux-driven rather than footer-text-driven
- structured input objects are the canonical compose model
- room send logic is driven by parsed chatbuf contents rather than draft text
  cleanup
- shared chatbuf behavior has an obvious home under `disco-chatbuf.el`
- shared message semantics have an obvious home under `disco-msg.el`
- shared thread semantics have an obvious home under `disco-thread.el`
- room-prefixed files exist only where the code is truly room-specific
- `emacs-qq` and `disco-room` are clearly converging on one shared chatbuf core
