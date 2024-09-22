;;; osx-pseudo-daemon.el --- Daemon mode that plays nice with OSX.

;; Author: Ryan C. Thompson
;; URL: https://github.com/DarwinAwardWinner/mac-pseudo-daemon
;; Version: 3.0
;; Package-Requires: ((pseudo-daemon "3.0"))
;; Created: 2013-09-20
;; Keywords: convenience osx

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

(define-obsolete-function-alias 'osx-pseudo-daemon-mode 'pseudo-daemon-mode "2.0")
(define-obsolete-variable-alias 'osx-pseudo-daemon-mode 'pseudo-daemon-mode "2.0")

(provide 'osx-pseudo-daemon)

;;; osx-pseudo-daemon.el ends here
