# Attachment telega-style refactor TODO

This checklist is intentionally scoped to work that can be completed in one
continuous refactor pass.

- [x] Move shared attachment/media helpers out of `disco-room.el` into
      `disco-media.el`:
  - attachment kind classification
  - display/meta label helpers
  - default save-name + download state helpers
  - download/open/play entry points
- [x] Add telega-style typed attachment inserters to `disco-ins.el`:
  - shared transfer/caption/url/preview leaf helpers
  - `document` inserter
  - `photo` inserter
  - `video` inserter
- [x] Rework room attachment rendering to use typed dispatch from
      `disco-room.el` while keeping room-only orchestration there
- [x] Add and update automated tests for the new media owner and typed inserter
      paths
- [x] Verify the refactor with byte-compilation and ERT

## Follow-up: spoiler media

- [x] Add attachment spoiler detection helpers to `disco-media.el`
- [x] Add spoiler placeholder inserter to `disco-ins.el`
- [x] Gate attachment rendering in `disco-room.el` on spoiler reveal state
- [x] Add spoiler attachment tests and re-run compile/ERT
