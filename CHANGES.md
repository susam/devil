Changelog
=========

0.2.0 (UNRELEASED)
------------------

### Added

- Add `, k` to the default list of repeatable keys.
- Add `, /` to the default list of repeatable keys.
- Add `, m m y` to the default list of repeatable keys.
- Add command `devil-show-version` to display Devil version.


### Changed

- Automatically detect the activation key and accumulate it in order
  to support `devil-mode-map` with multiple activation keys.


### Fixed

- Remove a stray `message` call.


0.1.0 (2023-05-07)
------------------

### Added

- Devil global and local minor modes.
- Default Devil key set to the comma (`,`).
- Special key `, ,` to type a literal comma.
- Special key `, SPC` to type a comma followed by a space.
- Special key `, RET` to type a comma followed by return.
- Translation rules that translate `,` and `, z` to `C-`.
- Translation rules that translate `m` and `, m m` to `M-`.
- Translation rule that translates `, ,` to `,`.
- Repeatable key sequences for `, p`, `, n`, `, f`, `, b`, `, m m f`,
  `, m m b`, and `, m x o`.
- Key binding for `isearch-mode-map` to support Devil key sequences in
  incremental search.
- Key binding for `universal-argument-map` to support repeating the
  universal argument with `u`.
