# Install Pebble Agenda 0.2.0

Pebble Agenda provides an Emacs package and a native Pebble Time 2 (Emery)
watch application. Emacs must remain running and reachable from the Pebble
phone app. The package uses your configured Org files only after you explicitly
enable its mode. Development and tests use isolated Emacs and synthetic files;
they never install into or modify your personal configuration.

## Install the Emacs package

1. Build with `scripts/release.sh`, or use its generated release artifacts.
2. In the Emacs instance that owns your agenda, run `M-x package-install-file`
   and select `dist/pebble-agenda-0.2.0.tar`.
3. Add the following to your configuration yourself, after your Org setup:

```elisp
(require 'pebble-agenda)
;; Defaults to your org-agenda-files; alternatively set explicit local files.
;; (setq pebble-agenda-files '("/path/to/tasks.org"))
(setq pebble-agenda-default-view 'today
      pebble-agenda-save-changes t)
(pebble-agenda-mode 1)
```

`examples/init.el` contains the complete example. The package requires Emacs
28.1+ and Org 9.5+. Installation/loading alone does not open agenda files, start
a listener, or change tasks. Enabling the global mode opens the configured
local Org files and starts the HTTP service. To stop it, run
`M-x pebble-agenda-mode`; existing Org buffers stay open.

## Install the watch application

Open `dist/pebble-agenda-0.2.0.pbw` with the Pebble phone app to sideload it.
Alternatively use the installed SDK's `pebble install --phone PHONE_IP` with
Developer Connection enabled. For local development/emulator testing:

```bash
scripts/build.sh
pebble install --emulator emery
```

The app is named **Pebble Agenda**. This release targets Emery (200 x 228), not
other screen sizes. Physical Android/watch behavior must be verified on your
own devices; emulator evidence cannot prove hardware compatibility.

## Pair the phone with Emacs

In Emacs, run `M-x pebble-agenda-pairing-info`. In the Pebble phone app, open
Pebble Agenda's Settings and enter the Emacs address and pairing token, then
save. The settings page is embedded in the watch app; no hosted website is
needed. The phone stores the credentials in its app-local storage. Tokens are
not included in the PBW or sent in URL query parameters.

The token is generated when the mode is enabled and kept in
`pebble-agenda-token-file` (normally `~/.emacs.d/pebble-agenda-token`, permissions
0600). Keep it private. To rotate it: disable the mode, remove that token file,
re-enable the mode, and re-pair the phone.

### Emacs on Android, on the same phone

Keep the defaults:

```elisp
(setq pebble-http-host "127.0.0.1"
      pebble-http-port 8765
      pebble-agenda-public-url "http://127.0.0.1:8765")
```

Loopback in the phone-side JavaScript reaches Emacs on that phone. Emacs and
the Pebble app both need to remain running; Android background restrictions
can interrupt the connection. Try a synthetic file on hardware before enabling
your own agenda files.

### Emacs on a desktop or laptop

`127.0.0.1` on the phone does not reach your desktop. Keep Emacs bound to
loopback and provide a phone-reachable HTTPS reverse proxy or secure tunnel
that forwards to `127.0.0.1:8765`, including the Authorization header. Set:

```elisp
(setq pebble-agenda-public-url "https://YOUR-EMACS-ENDPOINT")
```

Enter that same endpoint in phone Settings. The package does not provision
networking, cloud accounts, or certificates. For an explicitly trusted LAN you
can opt into `pebble-http-host "0.0.0.0"` and use the desktop's LAN address;
a token is mandatory for non-loopback listeners. A token authenticates requests
but does not encrypt plain HTTP. Use TLS/tunneling across untrusted networks.

## Using the watch

Up/Down moves and scrolls locally. Select opens a task, then selects a field:

- **State:** choose one of that file's actual TODO states, including done states
  and custom states such as NEXT or WAIT. Reopen a task through the Done view.
- **Scheduled / Deadline:** Today, Tomorrow, In one week, or Clear date.
- **Priority:** choose from the Org buffer's configured letter priority range.

Back from an action menu returns to task details; Back from details returns to
the list. List screens include page navigation and **Views / refresh**. Today
shows overdue/today scheduled tasks or deadlines; Upcoming includes the next
seven days plus overdue tasks; Open includes all unfinished tasks; Done includes
completed tasks. Completed items leave the Open/Today views immediately.
Undated tasks are available in Open. Archived and commented headings are skipped.

The package refreshes every ten seconds by default, including external edits
to unmodified buffers. A changed source returns the watch to the refreshed list.
Use `M-x pebble-agenda-refresh` or the watch's Refresh action for an immediate
refresh. Dates use the Emacs instance's `org-today` and timezone.

## Persistence and conflicts

By default each watch edit saves its source buffer. To avoid saving unrelated
work, watch actions refuse files with pre-existing unsaved edits, changed task
snapshots, external disk conflicts, removed sources, or read-only buffers/files.
Save or reconcile those edits in Emacs, then refresh the watch and try again.
The watch shows an Action unavailable screen on failure.

Org TODO states, blockers, hooks, repeat behavior, and CLOSED timestamps are
used. Watch actions cannot enter free-form log notes. Interactive notes and
state-change/reschedule/deadline logs are suppressed for unattended edits;
completion logging records a CLOSED timestamp when enabled. Hooks are ordinary
Emacs Lisp and can still impose their own restrictions. If a blocker prevents
a state change, the package rolls back the buffer edit and reports it.

Set `pebble-agenda-save-changes` to nil only if you intentionally want changes
to remain unsaved in Emacs. The package supports local unencrypted Org files;
remote/TRAMP and .gpg files are rejected to avoid unattended credential prompts.

## Reliability and limits

The phone authenticates requests with a bearer token. Events carry the committed
scene's session/revision and an idempotency key. Stale actions are rejected;
retries of an acknowledged edit do not run it twice. The watch retries button
events until acknowledged and blocks additional Select actions while pending.
When disconnected it displays a banner, retains the last scene for local
scrolling, and disables mutations until a successful reconnect.

The renderer supports 128 elements, 63 UTF-8 bytes per text record, and a fixed
font. Titles are shortened for display, and pagination bounds scene size.
Date menus offer presets, not arbitrary date entry; there is no text editing,
refiling, capture, or arbitrary Lisp evaluation endpoint. One paired watch is
supported per Emacs server. Each published replacement resets list selection.

## Reproduce tests and builds

```bash
scripts/test.sh                    # isolated ERT, JS, native sanitizer checks
scripts/package-emulator-test.sh   # temporary Org files, real watch controls
scripts/release.sh                 # package archive, PBW, install smoke test
```

The emulator test pairs through actual configuration events, performs durable
edits to temporary files, and stores screenshots under
`tests/screenshots/package/`. Never substitute your real agenda for test data.
