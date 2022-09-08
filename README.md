

# My GNU Emacs Configuration


# Table of Contents

1.  [My GNU Emacs Configuration](#orgac8dcc0)
2.  [Note](#orgfb29d93)
3.  [Packages Used (MELPA)](#org331c82b)
4.  [Libraries](#org93aca9b)
    1.  [cppimmo/cppimmo-xml.el](#orge35921a)
    2.  [cppimmo/cppimmo-count-words-mode.el](#org93ee385)
5.  [Key Bindings](#org4551568)
6.  [License](#org154541e)

This repository is for versioning my GNU Emacs configuration files.  I hope to
keep them platform independent, so I can use them across all my machines (both
UNIX-like and Windows).


# Note

To use this configuration there should be no *~/.emacs* file as this
configuration uses an *init.el* that is stored in the *~/.emacs.d*
directory.  Also ensure that when cloning this repository something resembling
following command is used:

    mv ~/.emacs.d ~/.emacs.d.bak && git clone <url> ~/.emacs.d

This configuration uses custom.el as the customize options settings file.


# Packages Used (MELPA)

-   use-package
-   pomodoro
-   php-mode
-   lua-mode
-   ox-leanpub
-   markdown-mode
-   markdown-preview-eww
-   2048-game
-   doom-themes


# Libraries


## cppimmo/cppimmo-xml.el

Contains functions for working creating my blog posts and RSS feed blog post entry items.  See "Key Bindings".


## cppimmo/cppimmo-count-words-mode.el

Wraps the functionality provided by the default count-words- functions.  See "Key Bindings".


# Key Bindings

**Globals**

-   "C-M-y" - Toggle built-in whitespace minor mode.

**cppimmo-insert-xml Library Functions (nxml-mode)**

-   "C-c M-!" - Insert XML CDATA tag for XML documents (I use this for styling my blog).
-   "C-c M-@" - Insert blog tags for XML documents (blog posts).
-   "C-c M-#" - Insert XML tags for RSS feed item (blog posts).

**cppimmo-count-words-mode**

-   "C-c C-b" - (cppimmo-count-words-buffer)
-   "C-c C-r" - (cppimmo-count-words-region)


# License

BSD 2-Clause License

Copyright (c) 2022, Brian Hoffpauir
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1.  Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

2.  Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

