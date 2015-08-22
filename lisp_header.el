# -*- mode: snippet; require-final-newline: nil -*-
# name: lisp-header-info
# key: lheader
# binding: direct-keybinding
# --

;;; `(file-name-nondirectory buffer-file-name)` --- $1 -*- lexical-binding: t -*-

;; Copyright (C) `(format-time-string "%Y")` `user-full-name`

;; Author: `(format "%s <%s>" user-full-name user-mail-address)`
;; Created: `(format-time-string "%d %b %Y")`
;; Version: 0.0.1
;; Keywords: $2
;; X-URL: https://github.com/`user-login-name`/${3:`(file-name-base buffer-file-name)`}

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; $0

;;; Code: