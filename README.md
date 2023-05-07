Devil Mode
==========

Devil mode trades your comma key in exchange for a modifier-free
editing experience! Yes, the comma key! The key you would normally
wield for punctuation in nearly every corner of text. Yes, this is
twisted! It would not be called the Devil otherwise, would it? If it
were any more rational, we might call it something divine, like, uh,
the God mode? But alas, there is nothing divine to be found here.
Welcome, instead, to the realm of the Devil! You will be granted the
occasional use of the comma key for punctuation, but only if you can
charm the Devil! But beware, for in this sinister domain, you must
relinquish your comma key and embrace an editing experience that
whispers wicked secrets into your fingertips!


Contents
--------

* [Introduction](#introduction)
* [Notation](#notation)
* [Get Started](#get-started)
* [Use Devil](#use-devil)
* [Typing Commas](#typing-commas)
* [Devil Reader](#devil-reader)
* [Translation Rules](#translation-rules)
* [Translation Examples](#translation-examples)
* [Extra Key Bindings](#extra-key-bindings)
* [Local Mode](#local-mode)
* [Custom Appearance](#custom-appearance)
* [Custom Devil Key](#custom-devil-key)
* [Why?](#why)
* [Support](#support)
* [Channels](#channels)
* [More](#more)


Introduction
------------

Devil mode intercepts our keystrokes and translates them to Emacs key
sequences according to a configurable set of translation rules. For
example, with the default translation rules, when we type `, x , f`,
Devil translates it to `C-x C-f`.

The choice of the comma key (`,`) to mean the control modifier key
(`C-`) may seem outrageous. After all, the comma is a very important
punctuation both in prose as well as in code. Can we really get away
with using `,` to mean the `C-` modifier? It turns out, this terrible
idea can be made to work without too much of a hassle. At least it
works for me! It might work for you too. If it does not, Devil can be
configured to use another key instead of `,` to mean the `C-`
modifier. See the section [Custom Devil Key](#custom-devil-key) for an
example.

A sceptical reader may rightfully ask: If `,` is translated to `C-`,
how on earth are we going to insert a literal `,` into the text when
we need to? The section [Typing Commas](#typing-commas) answers this.
But before we get there, we have some fundamentals to cover. Take the
plunge and see what unfolds! Maybe you will like this! Maybe you will
not! If you do not like this, you can always retreat to God mode, Evil
mode, the vanilla key bindings, or whatever piques your fancy!


Notation
--------

A quick note about the notation used in the document: The previous
example shows that `, x , f` is translated to `C-x C-f`. What this
really means is that the key sequence
<kbd>,</kbd><kbd>x</kbd><kbd>,</kbd><kbd>f</kbd> is translated to
<kbd>ctrl</kbd>+<kbd>x</kbd> <kbd>ctrl</kbd>+<kbd>f</kbd>. We do not
really type any space after the commas. The key <kbd>,</kbd> is
directly followed by the key <kbd>x</kbd>. However, the key sequence
notation used in this document contains spaces between each keystroke.
This is consistent with how key sequences are represented in Emacs in
general and how Emacs functions like `key-description`,
`describe-key`, etc. represent key sequences. When we really need to
type a space, it is represented as `SPC`.


Get Started
-----------

To get started quickly with Devil, clone its Git repository to your
system and load it in your Emacs initialization file with the
following steps:

 1. Clone Devil to your system:

    ```sh
    git clone https://github.com/susam/devil.git
    ```

 2. Add the following to your Emacs initialization file (i.e., to your
    `~/.emacs` or `~/.emacs.d/init.el` or `~/.config/emacs/init.el`):

    ```elisp
    (add-to-list 'load-path "/path/to/devil/")
    (require 'devil)
    (global-devil-mode)
    (global-set-key (kbd "C-,") 'global-devil-mode)
    ```

 3. Start the Emacs editor. Devil mode should now be enabled in all
    buffers. The modeline of each buffer should show the `Devil`
    lighter.

 4. Type `, x , f` and watch Devil translate it to `C-x C-f` and
    invoke the corresponding command.

 5. Type `C-,` to disable Devil mode. Type `C-,` again to enable it.


Use Devil
---------

Assuming vanilla Emacs key bindings have not been changed and Devil
has not been customised, here are some examples that demonstrate how
Devil may be used:

 1. Type `, x , f` and watch Devil translate it to `C-x C-f` and
    invoke the find file functionality.

 2. Type `, p` to move up one line.

 3. To move up multiple lines, type `, p p p` and so on. Some Devil
    key sequences are repeatable keys. The repeatable Devil key
    sequences can be repeated by typing the last key of the Devil key
    sequence over and over again.

 4. Another example of a repeatable Devil key sequence is `, f f f`
    which moves the cursor word by multiple characters.

 5. Type `, s` and watch Devil translate it to `C-s` and invoke
    incremental search.

 6. Type `, m s` and watch Devil translate it to `C-M-s` and invoke
    regular-expression-based incremental search. Yes, `m` is
    translated to `M-`.

 7. Type `, m m x` and watch Devil translate it to `M-x` and invoke
    the corresponding command.

 8. Type `, u , f` and watch Devil translate it to `C-u C-f` and move
    the cursor forward by 4 characters.

 9. Type `, u u , f` and the cursor moves forward by 16 characters.
    Devil uses its translation rules and an additional keymap to make
    the input key sequence behave like `C-u C-u C-f` which moves the
    cursor forward by 16 characters.

10. Type `, SPC` to type a comma followed by space. This is a special
    key sequence to make it convenient to type a comma in the text.
    Note that this sacrifices the use of `, SPC` to mean `C-SPC` which
    could have been a convenient way to set a mark.

11. Type `, z SPC` and watch Devil translate it to `C-SPC` and set a
    mark. Yes, `, z` is translated to `C-` too.

12. Similarly, type `, RET` to type a comma followed by the return
    key. This is another special key.

13. Type `, ,` to type a single comma. This special key is useful for
    cases when you really need to type a single literal comma.


Typing Commas
-------------

Devil makes the questionable choice of using the comma as its trigger
key. As illustrated in the previous section, typing `, x , f` produces
the same effect as typing `C-x C-f`. One might naturally wonder how
then we are supposed to type literal commas.

Most often when we edit text, we do not really type a comma in
isolation. Often we immediately follow the comma with a space or a
newline. This assumption usually holds good while editing regular
text. However, this assumption may not hold in some situations, like
while working with code when we need to add a single comma at the end
of an existing line.

In scenarios where the above assumption holds good, typing `, SPC`
inserts a comma and a space. Similarly, typing `, RET` inserts a comma
and a newline.

In scenarios, when we do need to type a single comma, type `, ,` instead.

Also, it is worth mentioning here that if all this fiddling with the
comma key feels clumsy, we could always customise the Devil key to
something else that feels better. We could also disable Devil mode
temporarily and enable it again later with `C-,` as explained in
section [Get Started](#get-started).


Devil Reader
------------

The following points briefly describe how Devil reads Devil key
sequences, translates them to Emacs key sequences, and runs commands
bound to the key sequences:

 1. As soon as the Devil key is typed (which is `,` by default), Devil
    wakes up and starts reading Devil key sequences. Type `C-h v
    devil-key RET` to see the current Devil key.

 2. After each keystroke is read, Devil checks if the key sequence
    accumulated is a special key. If it is, then the special command
    bound to the special key is executed immediately. Note that this
    step is performed before any translation rules are applied to the
    input key sequence. This is how the Devil special key sequence `,
    SPC` inserts a comma and a space. Type `C-h v
    devil-special-keys RET` to see the list of special keys and
    the commands bound to them.

 3. If the key sequence accumulated so far is not a special key, then
    Devil translates the Devil key sequence to a regular Emacs key
    sequence. If the regular Emacs key sequence turns out to be a
    complete key sequence and some command is found to be bound to it,
    then that command is executed immediately. This is how the Devil
    key sequence `, x , f` is translated to `C-x C-f` and the
    corresponding binding is executed. If the translated key sequence
    is a complete key sequence but no command is bound to it, then
    Devil displays a message that the key sequence is undefined. Type
    `C-h v devil-translations RET` to see the list of translation
    rules.

 4. After successfully translating a Devil key sequence to an Emacs
    key sequence and executing the command bound to it, Devil checks
    if the key sequence is a repeatable key sequence. If it is found
    to be a repeatable key sequence, then Devil sets a transient map
    so that the command can be repeated merely by typing the last
    keystroke of the input key sequence. This is how `, p p p` moves
    the cursor up by three lines. Type `C-h v devil-repeatable-keys
    RET` to see the list of repeatable Devil key sequences.

The variables `devil-special-keys`, `devil-translations`, and
`devil-repeatable-keys` may contain keys or values with the string
`%k` in them. This is a placeholder for `devil-key`. While applying
the special keys, translation rules, or repeat rules, each `%k` is
replaced with the actual value of `devil-key` before applying the
rules.


Translation Rules
-----------------

The following points provide an account of the translation rules that
Devil follows in order to convert a Devil key sequence entered by the
user to an Emacs key sequence:

 1. The input key vector read from the user is converted to a key
    description (i.e., the string functions like `describe-key`,
    `key-description`, produce). For example, if the user types
    <kbd>,</kbd><kbd>x</kbd><kbd>,</kbd><kbd>f</kbd>, it is converted
    to `, x , f`.

 2. Now the resulting key description is translated with simple string
    replacements. If any part of the string matches a key in
    `devil-translations`, then it is replaced with the corresponding
    value. For example, `, x , f` is translated to `C- x C- f`. Then
    Devil normalises the result to `C-x C-f` by removing superfluous
    spaces after the modifier keys.

 3. However, if the simple string based replacement leads to an
    invalid Emacs key sequence, it skips the replacement that causes
    the resulting Emacs key sequence to become invalid. For example `,
    m ,` results in `C-M-C-` after the simple string replacement
    because the default translation rules replace `,` with `C-` and
    `m` with `M-`. However, `C-M-C-` is an invalid key sequence, so
    the replacement of the second `,` to `C-` is skipped. Therefore,
    the input `, m ,` is translated to `C-M-,` instead.


Translation Examples
--------------------

By default, Devil supports a small but peculiar set of translation
rules that can be used to avoid modifier keys while typing various
types of key sequences. See `C-h v devil-translations RET` for the
translation rules. Here are some examples that demonstrate the default
translation rules. The obvious ones are shown first first. The more
peculiar translations come later in the table.

| Input     | Translated | Remarks                            |
|-----------|------------|------------------------------------|
| `, s`     | `C-s`      | `,` is replaced with `C-`          |
| `, m s`   | `C-M-s`    | `m` is replaced with `M-`          |
| `, z s`   | `C-SPC`    | `, z` is replaced with `C-` too    |
| `, z z`   | `C-z`      | ditto                              |
| `, m m x` | `M-x`      | `, m m`  is replaced with `M-` too |
| `, c , ,` | `C-c ,`    | `, ,` is replaced with `,`         |

Note how we cannot use `, SPC` to set a mark because that key sequence
is already reserved as a special key sequence in `devil-special-keys`,
so Devil translates `, z` to `C-` too, so that we can still type
`C-SPC` using `, z s` and set a mark.

Also, note how the translation of `, m m` to `M-` allows us to enter a
key sequence that begins with the `M-` modifier key.


Extra Key Bindings
------------------

Devil adds the following additional key bindings only when Devil is
enabled globally with `global-devil-mode`:

- Adds the Devil key to `isearch-mode-map`, so that Devil key
  sequences work in incremental search too.

- Adds `u` to `universal-argument-more` to allow repeating the
  universal argument command `C-u` simply by repeating `u`.

As mentioned before these features are available only when Devil is
enabled globally with `global-devil-mode`. If Devil is enabled locally
with `devil-mode`, then these features are not available.


Local Mode
----------

While the section [Get Started](#get-started) shows how we enable
Devil mode globally, this section shows how we can enable it locally.
Here is an example initialization code that enables Devil locally only
in text buffers.

```elisp
(add-to-list 'load-path "/path/to/devil/")
(require 'devil)
(add-hook 'text-mode-hook 'devil-mode)
(global-set-key (kbd "C-,") 'devil-mode)
```

This is not recommended though because this does not provide a
seamless Devil experience. For example, with Devil enabled locally in
a text buffer like this, although we can type `, x , f` to launch the
find-file minibuffer, we cannot use Devil key sequences in the
minibuffer. Further the special keymaps described in the previous
section work only when Devil is enabled globally.


Custom Appearance
-----------------

The following initialization code shows how we can customise Devil to
show a Devil smiley (😈) in the modeline and the echo area.

```elisp
(add-to-list 'load-path "/path/to/devil/")
(require 'devil)
(setq devil-lighter " \U0001F608")
(setq devil-prompt "\U0001F608 %t")
(global-devil-mode)
(global-set-key (kbd "C-,") 'global-devil-mode)
```

This is how Emacs may look if emojis are rendered correctly:

[![Screenshot of Emacs with Devil smiley][smiley-screenshot]][smiley-screenshot]

[smiley-screenshot]: https://i.imgur.com/oYtwnGi.png


Custom Devil Key
----------------

The following initialization code shows how we can customise Devil to
use a different Devil key.

```elisp
(add-to-list 'load-path "/path/to/devil/")
(setq devil-key "<left>")
(require 'devil)
(global-devil-mode)
(global-set-key (kbd "C-<left>") 'global-devil-mode)
```

The above example sets the Devil key to the left arrow key, perhaps
another dubious choice for the Devil key. With this configuration, we
can use `<left> x <left> f` and have Devil translate it to `C-x C-f`.

To customise the special keys, translation rules, and repeatable keys,
see the variables `devil-special-keys`, `devil-translations`, and
`devil-repeatable-keys`, respectively.


Why?
----

Why go to the trouble of creating and using something like this? Why
not just remap <kbd>caps lock</kbd> to <kbd>ctrl</kbd> like every
other sane person does? Or if it is so important to avoid modifier
keys, why not use something like God mode?

Well, this minor mode began as a tiny little experiment just for fun.
From the outset, it was clear that using something as crucial as the
comma for specifying the modifier key is asking for trouble. However,
I still wanted to see how far I could go with it. It turned out that
in a matter of days, I was using it full-time for all of my Emacs
usage.

This experiment was partly motivated by Macbook keyboards which do not
have a right <kbd>ctrl</kbd> key. Being a touch-typist myself, I found
it inconvenient to type key combinations like `C-x`, `C-a`, `C-w`,
`C-s`, etc. where both the modifier key and the modified key need to
be pressed with the left hand fingers. I am not particularly fond of
remapping <kbd>caps lock</kbd> to behave like <kbd>ctrl</kbd> because
that still suffers from the problem that key combinations like `C-x`,
`C-a` require pressing both the modifier key and the modified key with
the left hand fingers. I know many people remap both their <kbd>caps
lock</kbd> and <kbd>enter</kbd> to behave like <kbd>ctrl</kbd>. While
I think that is a fine solution, I was not willing to put up with the
work required to make that work seamlessly across all the various
operating systems I work on.

What began as a tiny whimsical experiment a few years ago turned out
to be quite effective, at least to me. I like that this solution is
implemented purely as Elisp and therefore does not have any external
dependency. I am sharing this solution here in the form of a minor
mode, just in case, there is someone out there who might find this
useful too.


Support
-------

To report bugs, suggest improvements, or ask questions,
[create issues][ISSUES].

[ISSUES]: https://github.com/susam/devil/issues


Channels
--------

The author of this project hangs out at the following places online:

  - Website: [susam.net](https://susam.net)
  - Mastodon: [@susam@mastodon.social](https://mastodon.social/@susam)
  - Twitter: [@susam](https://twitter.com/susam)
  - GitHub: [@susam](https://github.com/susam)
  - Matrix: [#susam:matrix.org](https://app.element.io/#/room/#susam:matrix.org)
  - IRC: [#susam:libera.chat](https://web.libera.chat/#susam)

You are welcome to subscribe to, follow, or join one or more of the
above channels to receive updates from the author or ask questions
about this project.


More
----

See [Emacs4CL](https://github.com/susam/emacs4cl), a DIY quick-starter
kit to set up Emacs for Common Lisp programming.

See [Emfy](https://github.com/susam/emfy), a DIY quick-starter kit to
set up Emacs for general purpose editing and programming.
