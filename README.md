<!-- -*- mode: gfm; truncate-lines: t; -*- -->

# Arthur's Emacs configuration #

> While any text editor can save your files, only Emacs can save your soul.
>     — [Per Abrahamsen](https://www.emacswiki.org/emacs/PerAbrahamsen)

Credits: This configuration is based off
[Steve Purcell's config](https://github.com/purcell/emacs.d), which I highly
recommend as a starter.

I have carefully tried to keep Emacs' start-up time to the minimum. All
modifications detailed below add approximately 0.2s start-up time compared to
Steve's original code (which took about 2.8s on my laptop). This is done by
deferring the loading of most (all?) new packages added, using `autoload`. The
trade-off is that you will notice a slight pause the first time you use that
mode (but not on subsequent times).

All notes below apply only to this configuration (i.e. not Steve's) unless
explicitly stated.

Note: Unless otherwise stated, "optional" means all other components, including
those listed here, will still work.

## Haskell ##

[Haskell-mode docs](https://haskell.github.io/haskell-mode/manual/latest/).

### External dependencies ###

  * (Optional) `hdevtools` executable for flycheck-hdevtools (in
    [Steve's config][purcell_flycheck-hdevtools]). No problems if `hdevtools` is
    not available.

[purcell_flycheck-hdevtools]: https://github.com/purcell/emacs.d/blob/ee9bd66d919b5acd93a49586f2039128fe474dc5/lisp/init-haskell.el#L26

  * (Optional) [`structured-haskell-mode`][structured-hs-mode_site] executable
    for nicer code indentation. If the executable `structured-haskell-mode` is
    not available, regular [`haskell-indentation-mode`][hs-mode_indent] will be
    used. The default colour scheme in `structured-haskell-mode` is hideous, so
    I've changed that if you're using the solarised-light theme. For now,
    colours do not change if you change themes unless `init.el` is re-evaluated
    or you restart Emacs.

    Note: The colours I've chosen are suitable only for light themes. I haven't
    found a way to detect light themes in general, so I've only enabled it for
    solarised-light.

[structured-hs-mode_site]: https://github.com/chrisdone/structured-haskell-mode
[hs-mode_indent]: https://haskell.github.io/haskell-mode/manual/latest/Indentation.html

## C/C++ ##

### External dependencies ###

  * ("Optional") `clang`. Without this, you have nothing. It's "optional" in the
    sense that not having `clang` wouldn't affect your, say, Haskell or poetry
    writing experience in Emacs. Were you looking for MS notepad with Emacs
    keybindings?

  * (Optional; requires `clang` and [`cmake`][cmake_site]) [`rtags`][rtags_site]
    for superior tagging. You will need to have the rtags daemon socket
    activated, using [launchd on OS X][rtags_OSX] or
    [systemd on Linux][rtags_linux]. Windows, start [`rdm` manually][rdm_manual]
    yourself. The daemon could be run within Emacs itself, but I just didn't
    like the idea.

    Note: `cmake-ide` does have [some code][cmake-ide_detect_rtags] to attempt
    to detect if `rdm` is already running on the system. However, it does not
    work on OS X (and probably Windows) because
    [`process-attributes`][emacs_man_processes] is not supported by Emacs on
    those platforms.

[cmake_site]: https://cmake.org/
[rtags_site]: https://github.com/Andersbakken/rtags
[rtags_OSX]: https://github.com/Andersbakken/rtags#integration-with-launchd-mac-os-x
[rtags_linux]: https://github.com/Andersbakken/rtags#integration-with-systemd-gnu-linux
[rdm_manual]: https://github.com/Andersbakken/rtags#usage
[cmake-ide_detect_rtags]: https://github.com/atilaneves/cmake-ide/blob/c227a23c3b6c02b2f1240ec5e496b63452a3111c/cmake-ide.el#L845
[emacs_man_processes]: https://www.gnu.org/software/emacs/manual/html_node/elisp/System-Processes.html

  * [`irony-mode`][irony-mode_site]. WIP.

[irony-mode_site]: https://github.com/Sarcasm/irony-mode

  * [`cmake-ide`][cmake-ide_site] for editing CMake configuration files.

[cmake-ide_site]: https://github.com/atilaneves/cmake-ide

## Python ##

Docs WIP.

## Projectile ##

Docs WIP.

  * (Optional) System utilities: `grep`, `cut`, `uniq`, `ag`

## Other functions ##

  * `kill-process-interactive`
  * `remote-shell`
  * `sudo-reopen-current-file`
