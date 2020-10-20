# Voicemacs

Voicemacs is a set of utilities for controlling Emacs by voice. Voicemacs doesn't add any voice recognition functionality itself - it's a tool that makes Emacs easier to use with an external voice control system, such as [Dragonfly](https://github.com/dictation-toolbox/dragonfly) or [Talon Voice](https://talonvoice.com/).

Voicemacs allows the voice rec system to call functions & commands remotely, and Voicemacs will send periodic updates with contextual information such as buffer contents, active modes, etc. Voicemacs also extends the functionality of some Emacs packages. For example, candidates in `dired`, `helm` & `company` are numbered and functions are exposed to manipulate them by their numbers, making them easier to navigate by voice. You can autocomplete or select a candidate by speaking its number.

Many of these features are personal hacks, polished for publishing. I find them robust but they are tailored for my workflow (for example, I only support `helm`, not `ido` or `ivy`). They may not work with your setup.

## How Does it Work?

Voicemacs is very much a work-in-progress. It's not stable or documented. The best way to understand it is probably to look at the client in my [Talon config](https://github.com/jcaw/talon_config/tree/master/emacs/utils), and the source code. Start with `voicemacs-base.el`.

With that said, here's an overview. The voice recognition software opens a persistent connection to Voicemacs over a TCP socket. Voicemacs uses this connection to send the client information on the state of Emacs as it changes, and the client can use it to send RPC requests. Additional state updates can be easily hooked, if you would like to send more information to your client.

## Installation

Right now, clone the repo directly. You'll have to wire it up yourself, perhaps using the modules from my [Talon config](https://github.com/jcaw/talon_config/tree/master/emacs/utils). This will probably become easier down the line.
