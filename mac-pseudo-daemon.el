;;; mac-pseudo-daemon.el --- Daemon mode that plays nice with Mac OS.

;; Author: Ryan C. Thompson
;; URL: https://github.com/DarwinAwardWinner/mac-pseudo-daemon
;; Version: 3.0
;; Package-Requires: ()
;; Created: 2013-09-20
;; Keywords: convenience osx mac

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package has been renamed to pseudo-daemon.el. Please use that
;; package instead.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'pseudo-daemon)

(define-obsolete-function-alias 'mac-pseudo-daemon-mode 'pseudo-daemon-mode "3.0")
(define-obsolete-variable-alias 'mac-pseudo-daemon-mode 'pseudo-daemon-mode "3.0")
(define-obsolete-variable-alias 'macpd-mac-frame-types 'pseudo-daemon-window-systems "3.0")

(provide 'mac-pseudo-daemon)

;;; mac-pseudo-daemon.el ends here
