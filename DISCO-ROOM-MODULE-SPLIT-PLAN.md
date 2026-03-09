# ownership-first split plan for `disco-room`

## Purpose

This document replaces the earlier split plan for `disco-room`.

The old version improved file boundaries, but it still treated `disco-room` as
if it were the default namespace for every extracted subsystem.  That is not the
architecture we actually want.

The new rule is:

- `disco-room` is a facade and public UI surface
- `disco-room` is not the default owner prefix for new internals
- code should move to the module that truly owns the behavior, even if it was
  extracted from `disco-room.el`

This is closer to what is useful in telega: not file-name mimicry, but
owner-based responsibility boundaries.

## Core judgment

The main problem with `disco.el/disco-room.el` is not only that it is large.
The bigger problem is that it became a historical bucket for code with very
mixed ownership.

Today it contains, in one file:

- room buffer lifecycle and mode wiring
- shared chatbuf-like input mechanics
- Discord-specific send/composer rules
- message model helpers
- thread prompts and thread update flows
- room search and jump logic
- timeline rendering and EWOC reconcile
- event-driven partial patching
- room menus and keymap glue

If we split that file into a pile of `disco-room-*` modules, we improve file
size but keep the wrong mental model: "all room-related code belongs to the
room namespace".

That is the part this new plan rejects.

## The naming decision

`disco-room` should remain only where it actually means "room buffer public
surface".

That means:

- keep `disco-room.el`
- keep public commands such as `disco-room-open` and `disco-room-mode`
- keep compatibility-facing command names where changing them would cause churn
- do not use `disco-room-*` as the default namespace for newly isolated
  internals

In practice there are three naming layers:

- `disco-` for package namespace
- owner namespace such as `chatbuf`, `msg`, `thread`, `media`, `embed`,
  `root`, or genuinely `room`
- command compatibility names for public interactive entry points

The second layer should drive the split.

## What we learn from telega

The useful lesson from telega is not "make more files named after chat".

The useful lesson is:

- chat buffer behavior has one owner
- message behavior has one owner
- insert/render helpers have one owner
- root/list UI has one owner
- event/update plumbing has one owner
- transient is an implementation detail of those owners, not a top-level split
  axis by itself

So the telega-style move here is:

- stop treating `disco-room.el` as the mother namespace
- start assigning code to its real owner

## Architectural target

The long-term target is not "many `disco-room-*` files".

The long-term target is:

- `disco-room.el` becomes a thin room facade
- shared chatbuf behavior grows under `disco-chatbuf`
- shared message/domain helpers grow under `disco-msg`
- shared thread helpers grow under `disco-thread`
- media/embed rendering lives with `disco-media` and `disco-embed` when it is
  not room-specific
- only the irreducibly room-specific parts keep `room` ownership

In other words:

- shared owner first
- room owner last

## Ownership rules

When a function currently lives in `disco-room.el`, ask these questions in
order.

### 1. Is it shared chatbuf behavior?

If it is about editable tail input, prompt markers, draft history, aux state,
input options, structured input objects, or chatbuf-local point discipline, it
belongs under `disco-chatbuf`.

Examples:

- input markers and bounds
- history navigation primitives
- prompt installation/update primitives
- aux state lifecycle
- structured input object storage conventions

It does not become room-owned just because `disco-room.el` happened to grow it
first.

### 2. Is it shared message/domain logic?

If it is about message identity, references, previews, normalized accessors,
forward/reply metadata, poll/reaction data shaping, or reusable message state
helpers, it belongs under `disco-msg`.

Examples:

- message reference extraction
- author/display derivation
- compact preview generation
- reusable poll/reaction state helpers

Interactive room commands are different; those may still remain room-facing even
if they call into `disco-msg` heavily.

### 3. Is it shared thread logic?

If it is about thread predicates, status derivation, prompt choices, update
application, or thread metadata shaping, it belongs under `disco-thread`.

Examples:

- archived/locked/private predicates
- auto-archive duration prompt helpers
- declarative thread update helpers

Only room-specific orchestration should remain outside that module.

### 4. Is it media/embed rendering that is not room-specific?

If the code is really about rendering one attachment/embed card or normalizing
that content, it should move toward `disco-media.el` or `disco-embed.el`
instead of creating more room-prefixed files.

### 5. Is it truly room-specific UI/render/update behavior?

Only after the earlier answers are no should code remain room-owned.

That usually means things such as:

- room EWOC lifecycle
- room timeline reconcile
- room-specific incremental patching
- room buffer keymap/context switching
- room buffer lifecycle glue
- room-only header/footer composition that depends on timeline state

This is the category where `room` still makes sense.

## What `disco-room` should own

`disco-room` should own the room UI surface, not every implementation detail.

Good long-term responsibilities for `disco-room`:

- `disco-room-mode`
- `disco-room-open`
- room-local state shape that defines buffer identity
- top-level room keymap assembly
- compatibility-facing public commands
- thin wrappers that route to the real owners
- room-level coordination between chatbuf, render, and event owners

Bad long-term responsibilities for `disco-room`:

- generic input-history internals
- structured input object storage semantics
- reusable message helpers
- generic thread prompt/update helpers
- attachment/embed rendering that is not actually room-specific
- every helper named `disco-room--something` just because the file is old

## Public API versus owner namespace

The split plan should distinguish these two things explicitly.

### Public API

Public interactive names may stay room-facing for stability.

Examples that can remain:

- `disco-room-open`
- `disco-room-mode`
- `disco-room-send-message`
- `disco-room-filter-search`
- `disco-room-reply-to-message`

This is about user-facing and compatibility-facing surface area.

### Owner namespace

Internal helpers should be named after the owner, not after the historical file
of origin.

Examples of the desired direction:

- `disco-chatbuf-*` for shared input mechanics
- `disco-msg-*` for shared message/domain helpers
- `disco-thread-*` for shared thread helpers
- `disco-ins-*` for shared insert/render leaf helpers
- `disco-room-events-*` for room live-update internals

Compatibility wrappers may keep older names during migration, but they should no
longer define the architecture.

## File strategy

Do not start by inventing a large family of `disco-room-*` files.

Start by expanding existing owners.

### Existing owners to grow first

#### `disco-chatbuf.el`

This is the first place to put shared chat-buffer behavior.

Grow it with:

- stable prompt/input marker behavior
- input history state and navigation primitives
- aux state lifecycle
- input options state lifecycle
- structured input object primitives
- prompt/footer update primitives that are generic enough to reuse

If this file becomes too large, a sibling such as `disco-chatbuf-input.el` is a
better direction than defaulting to `disco-room-compose.el`.

#### `disco-msg.el`

This should grow for shared message semantics.

Grow it with:

- more normalized message accessors
- forward/reply/reference helpers
- reusable preview helpers
- poll/reaction data shaping when not UI-specific

Do not turn it into a bag of room interactive commands, but do move pure message
logic here aggressively.

#### `disco-thread.el`

This should absorb shared thread semantics.

Grow it with:

- more thread status and capability derivation
- thread prompt choice helpers
- thread update application helpers

Room commands can still call into it.

#### `disco-media.el` and `disco-embed.el`

Move reusable render/normalize logic here instead of making everything a room
render helper.

#### `disco-ins.el` (new owner to introduce)

If we want telega-style render ownership, this is the one new top-level owner
that is justified.

It should own shared insert/render leaf helpers, analogous to the role
`telega-ins.el` plays in telega.

Good candidates:

- divider/header/footer leaf text builders
- message section insertion helpers
- reusable button/label insertion helpers
- reply/forward/media/poll/reaction section insertion helpers when they are not
  specific to EWOC orchestration itself

This is the right long-term home for render leaf helpers; they should not grow
into a `disco-room-render.el` bucket.

### Room-owned files that still make sense

Only a small number of room-owned files are structurally justified.

#### `disco-room.el`

This remains the facade and public surface.

It also keeps room timeline orchestration that is truly tied to the room buffer
itself, for example:

- EWOC lifecycle
- render-context computation
- point-preserving room redraw
- timeline reconcile
- coordination between room state and `disco-ins.el` leaf rendering

Do not introduce `disco-room-render.el` as a primary owner.  If a helper is a
leaf render helper, it should move to `disco-ins.el`; if it is room timeline
orchestration, it can remain in `disco-room.el`.

#### `disco-room-events.el`

This is justified because room live updates are tied to room buffer rendering
and invalidation policy.

It should own:

- gateway update hookup
- partial patch application
- room-local invalidation and rerender decisions
- optimistic room read/ack flows if they remain buffer-coupled

### Transitional files

A file extracted from `disco-room.el` is not automatically the right long-term
name.

Current example:

- `disco-room-search.el` is acceptable as an intermediate extraction boundary
- it should not be treated as proof that every future subsystem must be
  `disco-room-*`
- over time, shared pieces from that file should move to the real owners if
  they prove reusable

The point of a first extraction is decoupling, not freezing the final namespace.

## Modules we should not create by default

The old plan leaned too much toward names such as these:

- `disco-room-compose.el`
- `disco-room-message.el`
- `disco-room-thread-ui.el`
- transient-only files like `disco-room-transient.el`

This new plan does not treat those as default targets.

They are only justified if, after moving shared code into existing owners, there
is still a coherent room-only subsystem left over.

That means:

- do not create `disco-room-compose.el` just because compose code lived in
  `disco-room.el`; first separate shared chatbuf mechanics from Discord-specific
  send orchestration
- do not create `disco-room-message.el` just because many commands operate on a
  message; first move pure message logic into `disco-msg.el`
- do not create `disco-room-thread-ui.el` just because thread commands are long;
  first expand `disco-thread.el`
- do not create transient-only ownerless files; transient should stay with the
  subsystem that owns the operation
- do not create `disco-room-render.el`; telega-style render ownership wants an
  `ins` owner for leaf helpers and room orchestration to stay in
  `disco-room.el`

## Recommended ownership map for current `disco-room.el`

This is the practical answer to "where should the code go?"

| Current responsibility in `disco-room.el` | Real owner | Preferred destination |
| --- | --- | --- |
| input markers, prompt/input region rules, history mechanics | shared chatbuf | `disco-chatbuf.el` |
| aux state and input-options lifecycle | shared chatbuf | `disco-chatbuf.el` |
| structured input object storage conventions | shared chatbuf | `disco-chatbuf.el` |
| Discord payload shaping for send/edit/attach | room facade plus shared helpers | keep facade wrappers, push reusable parts down |
| message identity/reference/preview helpers | shared message domain | `disco-msg.el` |
| thread status, prompt parsing, update shaping | shared thread domain | `disco-thread.el` |
| attachment/embed card normalization and reusable render bits | media/embed owners | `disco-media.el`, `disco-embed.el` |
| divider/header/footer leaf render helpers and message section insertion helpers | shared insert/render owner | `disco-ins.el` |
| room search/jump/filter orchestration | transitional room adapter plus shared helpers | `disco-room-search.el` for now, then reevaluate |
| EWOC/timeline render orchestration | room-specific orchestration | keep in `disco-room.el` |
| live update patching and invalidation | room-specific | `disco-room-events.el` |
| keymap assembly and public commands | room facade | `disco-room.el` |

## Migration strategy

The migration strategy changes substantially under this ownership-first model.

The old instinct was: create more `disco-room-*` files.

The new instinct is:

1. move shared code into existing owners first
2. create room-owned files only for room-specific leftovers
3. keep `disco-room` as facade/compat surface while internals relocate

## Recommended migration order

### Milestone 0 - define the ownership rule

- accept that `disco-room` is facade/public API, not the mother namespace
- document this rule in file headers and planning docs
- allow compatibility wrappers while ownership moves underneath

Acceptance:

- contributors can answer "why does this live here?" in terms of owner, not
  historical source file

### Milestone 1 - keep extracting low-risk leaves, but do not canonize the prefix

- keep `disco-room-search.el` as a decoupling step
- rename internals by owner where that helps readability
- avoid assuming the current file name is the final architecture

Acceptance:

- behavior stays stable
- extraction lowers coupling even if the file name is temporary

### Milestone 2 - grow `disco-chatbuf.el` aggressively

Move or factor shared chatbuf behavior out of `disco-room.el`:

- input region ownership
- draft/history primitives
- aux/input-options storage
- structured input object primitives
- prompt/footer update primitives that are not Discord-specific

Acceptance:

- rich draft behavior remains intact
- attachment input objects still preserve text properties
- room code reads more like adapter code and less like chatbuf core

### Milestone 3 - move pure message logic into `disco-msg.el`

Move or factor shared message helpers out of room:

- reference accessors
- preview helpers
- author/display helpers
- reusable poll/reaction/forward state shaping

Acceptance:

- room interactive commands become thinner
- message semantics stop being hidden inside room code

### Milestone 4 - move shared thread logic into `disco-thread.el`

Move or factor shared thread helpers out of room:

- prompt parsing
- status/capability derivation
- update shaping helpers

Acceptance:

- thread UI commands become orchestration wrappers instead of state owners

### Milestone 5 - introduce an `ins`-style render owner

Create or expand `disco-ins.el` for render leaf helpers, and do not create
`disco-room-render.el`.

Move or factor out:

- divider/header/footer leaf text builders
- message/section insertion helpers
- reusable reply/forward/media/poll/reaction insertion helpers
- other render leaves that do not own EWOC lifecycle or timeline reconcile

Keep in `disco-room.el` for now:

- EWOC lifecycle
- room render contexts
- point-preserving room redraw
- timeline reconcile

Acceptance:

- render leaf helpers are reviewable without wading through composer/search/
  thread logic
- `disco-room.el` keeps only orchestration, not a growing pile of insert leaves

### Milestone 6 - isolate room-specific live updates

Create or expand `disco-room-events.el` for:

- gateway callbacks
- partial update application
- invalidation policies
- optimistic room read/ack patching when buffer-coupled

Acceptance:

- event logic is reviewable without mixing it into rendering and input code

### Milestone 7 - revisit transitional room-owned extractions

After shared owners and room-specific owners stabilize:

- review `disco-room-search.el`
- review remaining room-prefixed helper families
- move shared pieces again if they are not truly room-owned

Acceptance:

- room-prefixed files are room-owned by design, not by accident

### Milestone 8 - shrink `disco-room.el` to a real facade

At the end:

- keep room mode, open, keymap assembly, public command routing, and
  compatibility wrappers in `disco-room.el`
- remove deep implementation blocks that belong to other owners

Success criterion:

- `disco-room.el` feels like a front door, not like the whole house

## Naming rules for new code

Use these rules for new internals.

- keep `disco-room-*` only for public room commands, room mode, room facade
  helpers, and truly room-specific owners
- prefer `disco-chatbuf-*` for shared chatbuf mechanics
- prefer `disco-msg-*` for shared message/domain helpers
- prefer `disco-thread-*` for shared thread helpers
- prefer `disco-ins-*` for shared insert/render leaf helpers
- prefer `disco-media-*` / `disco-embed-*` when the code is really about those
  content types
- use a room-prefixed internal namespace only when the implementation is tied to
  room EWOC, room live updates, or room buffer-specific coordination

When in doubt, ask:

- would this code still make sense if `disco-room.el` disappeared and room were
  rebuilt as a thin adapter?

If yes, it probably should not be owned by `disco-room`.

## Transient rule

Transient is not an owner.

Keep transient definitions with the subsystem that owns the underlying action.

That means:

- room-wide menus can stay in `disco-room.el` while room is still the facade
- compose-related transients belong with chatbuf/compose ownership
- message-operation transients belong with the message-action owner
- root transients belong with root ownership
- `disco-transient.el` should only contain shared transient infrastructure when
  such infrastructure is actually shared

## Test strategy

Tests should follow ownership, not historical filename origin.

That means the target test layout is not automatically a mirror of
`disco-room-*` files.

Preferred direction:

- expand `test/disco-chatbuf-test.el` for shared chatbuf mechanics
- add or expand `test/disco-msg-test.el` when message-domain helpers grow enough
- add or expand `test/disco-thread-test.el` when thread helper coverage grows
- keep room integration tests in `test/disco-room-test.el`
- add room-specific render/event suites only for code that remains truly
  room-owned

Rule:

- if code moves to a shared owner, its tests should move there too
- `test/disco-room-test.el` should become integration-heavy, not the default
  home for every detail

## Risks

### 1. Keeping the old architecture under new filenames

Risk:

- we create more files but still think in one giant `disco-room` namespace

Mitigation:

- review each move by asking for the real owner, not just a destination file

### 2. Polluting shared modules with room-specific orchestration

Risk:

- in reaction to the old problem, we move too much imperative UI glue into
  `disco-chatbuf.el`, `disco-msg.el`, or `disco-thread.el`

Mitigation:

- only move code downward when the ownership is genuinely shared
- leave room-specific orchestration at the room facade/render/events layers

### 3. Namespace churn without user value

Risk:

- we rename public commands too early and create needless compatibility churn

Mitigation:

- keep public `disco-room-*` entry points stable while internals are reorganized

### 4. Transitional files becoming permanent by inertia

Risk:

- an intermediate extraction such as `disco-room-search.el` gets treated as the
  final architecture simply because it already exists

Mitigation:

- explicitly mark transitional files as provisional in their headers and plans
- revisit them after shared owners stabilize

### 5. Cyclic dependencies

Risk:

- facade, render, events, and shared owners start to require each other

Mitigation:

- keep the facade as the coordinator
- move pure helpers downward into shared owners
- keep room render and room events separate from generic chatbuf internals

## What not to do

- do not split by line count alone
- do not assume extracted code must keep a `disco-room-*` name
- do not create `disco-room-compose.el`, `disco-room-message.el`, or
  `disco-room-thread-ui.el` by reflex
- do not use transient as a top-level split axis
- do not move Discord-specific room orchestration into shared owners just to
  avoid the word `room`
- do not rename public commands aggressively when a compatibility wrapper will
  do

## Immediate practical guidance

For the next round of work, the right question is not:

- "what should be the next `disco-room-*` file?"

The right questions are:

- what in `disco-room.el` is really `disco-chatbuf`?
- what in `disco-room.el` is really `disco-msg`?
- what in `disco-room.el` is really `disco-thread`?
- what remains room-specific after those moves?

Only the remainder deserves new room-owned files.

## Exit criteria

This plan is complete when all of the following are true:

- `disco-room.el` is primarily facade and public room API
- shared chatbuf behavior has an obvious home under `disco-chatbuf`
- shared message semantics have an obvious home under `disco-msg`
- shared thread semantics have an obvious home under `disco-thread`
- room-prefixed files exist only where the code is truly room-specific
- file names and helper prefixes reflect ownership instead of extraction origin
- future work no longer requires opening one giant room file just to change a
  shared behavior
