# Voicemacs

Voicemacs is a set of utilities for controlling Emacs by voice. Voicemacs doesn't add any voice recognition functionality itself - it's a tool that makes Emacs easier to use with an external voice control system, such as [Dragonfly](https://github.com/dictation-toolbox/dragonfly) or [Talon Voice](https://talonvoice.com/).

The core of Voicemacs is an RPC server. The speech client can use this to call functions & commands remotely, and extract contextual information such as buffer contents, active modes, etc. Voicemacs also extends native functionality and makes certain subsystems (`dired`, `helm`, `company`) easier to navigate by voice.

Many of these features are personal hacks, polished for publishing. They are tailored for my workflow. I find them robust, but they may not work with your setup.

## How Does it Work?

Voicemacs is very much a work-in-progress. It's not stable or documented. The best way to understand it is probably to look at the client in my [Talon config](https://github.com/jcaw/talon_config/tree/master/newapi/emacs/utils), and the source code. Start with `voicemacs-base.el`.

With that said, here's a rough overview. Voicemacs publishes an HTTP server which runs within Emacs, via [Porthole](https://github.com/jcaw/porthole). You interact by sending HTTP requests to the server. It's one-way, with Emacs as the destination, so when data is available to sync, Voicemacs will set a flag in your Emacs title. Ping Emacs to pull the new data.

This will probably change in the future. Ideally, the voice recognition system and Voicemacs will maintain a persistent socket connection, and the direction of control may be reversed.

## Installation

Right now, clone the repo directly. You'll have to wire it up yourself, perhaps using the modules from my [Talon config](https://github.com/jcaw/talon_config/tree/master/newapi/emacs/utils). This will probably become easier down the line.
