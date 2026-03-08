# disco-room telega chatbuf alignment plan

## Status

This plan updates the older `disco-room` refactor outline to match the current
shared goal for `disco.el` and `emacs-qq`.

It complements these documents:

- `emacs-qq/TELEGA-CHATBUF-ALIGNMENT-DESIGN.md`
- `emacs-qq/INCREMENTAL-CHAT-PLAN.md`

The earlier version of this file focused mostly on persistent EWOC and local
message invalidation.  Much of that groundwork is already in place.  The main
remaining gap is no longer timeline persistence; it is chatbuf/composer
architecture.

## Goal

Move `disco-room` toward the same telega-style chatbuf model now targeted for
both clients:

- one real tail input region in the buffer
- prompt and footer updated independently from the input contents
- reply/edit/forward-like state represented as aux state
- structured input objects instead of text-only draft conventions
- normal typing and undo behavior
- a shared chatbuf core in `disco.el` that `disco-room` and `qq-chat` both use

This is architectural alignment, not a visual clone of telega.

## What changed from the old plan

The following assumptions from the older plan no longer match the current
objective:

- the main blocker is not EWOC lifetime anymore; `disco-room` already keeps a
  persistent EWOC and message-node table in practice
- the next step is not "better footer composer updates"; the next step is to
  stop treating the composer as editable footer text at all
- attachment tokens should not be treated as the long-term model
- shared infrastructure should grow beyond `disco-chat-input.el` into a richer
  chatbuf core
- `emacs-qq` should not merely reuse the old input layer; both clients should
  converge on one chatbuf architecture

## What telega is doing differently

The important telega properties to align with are:

- the input region is the real tail of the chat buffer
- prompt and footer are separate update surfaces
- reply/edit live in a dedicated aux state object
- input can contain structured objects via text properties
- send logic parses the input region into backend-native payloads
- prompt/footer changes do not require rebuilding the editable input region

## Current disco-room state

### Already present

`disco-room` already has substantial timeline groundwork:

- persistent EWOC timeline
- persistent message-node table
- render-context table keyed by message id
- partial patching for ordinary message changes
- point-preserving frame and timeline updates
- local invalidation for several media and message dependency cases
- initial shared chatbuf skeleton now lives in `disco-chatbuf.el`

### Still misaligned with the target

The remaining misalignment is concentrated in composer/input architecture:

- editable input still comes from footer-composed text via `disco-chat-input.el`
- prompt, footer, and input still behave like one coupled surface
- reply/edit state is split across room-local variables instead of a common aux
  state model
- attachments still use visible draft tokens like `[file:n]`
- send logic is still string-first and then strips token syntax back out
- the shared input layer is still too weak to serve as the long-term core for
  both `disco-room` and `qq-chat`

## Target architecture

### 1. Shared chatbuf core in `disco.el`

A shared module, `disco-chatbuf.el`, becomes the default place for chatbuf
composer behavior.

It should own:

- stable prompt/input markers
- prompt button lifecycle
- tail-input management
- input history behavior
- aux state lifecycle
- input-options state lifecycle
- structured input object insertion and repair
- shared prompt/footer update primitives

It should not own:

- room timeline rendering details
- Discord-specific permission logic
- Discord-specific message serialization rules

### 2. Real tail input region

`disco-room` should stop rebuilding an editable draft region through EWOC footer
properties.

Instead:

- the footer stays read-only UI
- the prompt is inserted and updated independently
- the editable input begins at a stable marker after the prompt
- current draft contents are the real buffer text from input marker to point max

This removes the need for:

- footer-property rebinding for the input region
- synthetic newline handling for empty drafts
- special logical-end behavior caused by synthetic footer input

### 3. Prompt and footer split

Prompt and footer should become separate surfaces.

Prompt should reflect compact send identity and mode information.
Footer should host read-only modules such as:

- typing indicator
- restriction reason
- aux summary when useful
- room-specific status lines
- future input options summary

### 4. Aux state becomes first-class

The current mix of `disco-room--pending-reply-to`, `disco-room--pending-edit`,
and footer text should converge on a shared aux model.

The shared aux object should cover at least:

- aux type
- primary message/source object
- optional aux metadata
- saved draft state where needed

`disco-room` may keep compatibility wrappers while migrating, but send, cancel,
and display should move toward aux-driven behavior.

### 5. Structured input objects replace attachment tokens

Visible attachment token syntax should be treated as transitional only.

The long-term model is telega-style structured input objects stored as text
properties on inserted display text.

For `disco-room`, likely object kinds include:

- attachment
- forward stub or forward selection later
- poll draft or poll option stub later
- future send-option objects where useful

### 6. `disco-room` becomes a chatbuf adapter

After migration, `disco-room` should primarily provide:

- room-specific prompt/footer content
- room-specific aux rendering text
- room-specific send serialization from chatbuf contents
- room-specific permission and capability rules
- room timeline rendering and invalidation

The generic chatbuf mechanics should live below it.

## Migration phases

### Phase 0 - timeline groundwork mostly done

This phase is effectively already in place:

- persistent EWOC lifetime
- message-node reuse
- render-context tracking
- partial message invalidation for many routine changes

These are no longer the critical path for telega alignment.

### Phase 1 - shared chatbuf core skeleton

Status: started.

Deliverables:

- `disco-chatbuf.el` exists
- shared prompt/input/history/aux/object helpers exist
- focused tests cover prompt lifecycle, object repair, history, and aux state

This phase is about stabilizing the shared API shape before migration of larger
buffers.

### Phase 2 - validate the shared core on the simpler client first

For lowest risk, the first concrete consumer should be `emacs-qq`.

Reason:

- `qq-chat` has fewer send variants
- OneBot segments are already a natural structured input target
- the shared core can be adjusted there before `disco-room` takes it on

This means `disco-room` should not race ahead with a room-specific rewrite that
bypasses the shared core.

### Phase 3 - migrate `disco-room` to real tail input

Deliverables:

- `disco-room` stops creating editable input through EWOC footer text
- prompt lifecycle moves to `disco-chatbuf.el`
- footer becomes read-only UI only
- room draft reads and writes go through the real tail input region

Expected win:

- simpler point handling
- fewer input-specific footer hacks
- closer telega alignment for normal typing behavior

### Phase 4 - move reply/edit to aux-driven behavior

Deliverables:

- reply and edit state map cleanly onto shared aux state
- cancel paths use common aux reset behavior
- prompt/footer display is derived from aux state
- room-local compatibility wrappers remain only where necessary

Expected win:

- cleaner room command model
- easier parity with `qq-chat`
- easier future extension for forward or richer compose modes

### Phase 5 - replace attachment tokens with structured input objects

Deliverables:

- `disco-room-attach-file` inserts an input object instead of textual token
- attachment listing, removal, and reorder operate on objects rather than token
  syntax
- draft display remains readable without exposing transport syntax

Expected win:

- no more token cleanup pass before send
- cleaner send pipeline
- much closer match to telega's input-object model

### Phase 6 - adapt send logic to parsed chatbuf contents

Deliverables:

- send logic consumes parsed chatbuf contents plus aux state
- string content, attachments, and future objects are serialized by a dedicated
  room adapter layer
- edit/send paths stop depending on token stripping

Expected win:

- cleaner send semantics
- easier future support for richer object kinds
- less incidental coupling between UI text and outbound payload shape

### Phase 7 - cleanup and consolidation

Deliverables:

- old `disco-chat-input.el` paths become compatibility shims or are removed
- prompt/footer update paths are clearly separate from timeline updates
- common chatbuf invariants are tested independently from room rendering tests

Expected win:

- one obvious place to evolve composer behavior in the future
- less duplicate work across `disco-room` and `qq-chat`

## Immediate implementation order

1. keep growing and testing `disco-chatbuf.el`
2. validate its API shape in `emacs-qq` first
3. migrate `disco-room` from footer-composed input to tail input
4. move reply/edit to aux-driven behavior
5. replace attachment tokens with structured objects
6. adapt send/edit logic to parsed chatbuf contents
7. only then remove old compatibility layers

Reason:

- shared API stability matters more than early room-specific cleverness
- tail input migration is the key architectural shift
- token removal is safest after tail input and aux state are already in place

## Risk areas

- temporarily coexisting footer-composed and tail-input paths can make point
  preservation tricky during migration
- attachment editing and message editing have different backend constraints and
  should not be conflated
- `disco-room` has more send variants than `qq-chat`, so adapter boundaries
  must be designed before object support expands too far
- width changes and some structural view changes may still require full visible
  rerender even after chatbuf migration

## Success criteria

We can consider this plan successful when all are true:

- typing in `disco-room` no longer depends on footer-property rebound input
- prompt and footer update independently from the editable input region
- reply/edit behavior is aux-driven rather than footer-text-driven
- attachments no longer depend on visible `[file:n]` tokens
- room send logic is driven by parsed chatbuf contents rather than draft text
  cleanup
- `emacs-qq` and `disco-room` are clearly converging on one shared chatbuf core
