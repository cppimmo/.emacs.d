                      ____________________________

                       MY GNU EMACS CONFIGURATION

                            Brian Hoffpauir
                      ____________________________


Table of Contents
_________________

1. Note
2. Packages Used (MELPA)
3. Key Bindings
4. License


This repository is for versioning my GNU Emacs configuration files. I
hope to keep them platform independent, so I can use them across all my
machines (both UNIX-like and Windows).


1 Note
======

  To use this configuration there should be no /~/.emacs/ file as this
  configuration uses an /init.el/ that is stored in the /~/.emacs.d/
  directory.  Also ensure that when cloning this repository something
  resembling following command is used:
  ,----
  | mv ~/.emacs.d ~/.emacs.d.bak && git clone <url> ~/.emacs.d
  `----
  This configuration uses custom.el as the customize options settings
  file.


2 Packages Used (MELPA)
=======================

  - use-package
  - pomodoro
  - php-mode
  - lua-mode
  - ox-leanpub
  - markdown-mode
  - 2048-game
  - doom-themes


3 Key Bindings
==============

  - "C-M-y" - Toggle built-in whitespace minor mode.
  - "C-M-!" - Insert XML CDATA tag for XML documents (I use this for
    styling my blog).


4 License
=========

  BSD 2-Clause License

  Copyright (c) 2022, Brian Hoffpauir All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
