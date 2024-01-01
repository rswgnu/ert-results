;;; ert-results.el --- Filter ERT test results display   -*- lexical-binding: t; -*-
;; Usage:        GNU Emacs Lisp Library
;; Keywords:     lisp, maint, tools
;; Version:      1.0.3
;;
;; Author:       Robert Weiner <rsw@gnu.org>
;;
;; Package-Requires: ((emacs "24.1"))
;; URL:              https://github.com/rswgnu/ert-results
;;
;; Orig-Date:    28-Dec-23 at 14:52:30
;; Last-Mod:      1-Jan-24 at 23:58:17 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2023  Free Software Foundation, Inc.
;;
;; Licensed under the GNU General Public License, version 3.
;;
;; This file is not yet part of Emacs.  It requires Emacs 27.1 or above.
;;
;;; Commentary:
;;
;; See documentation for `ert-select-tests' for test SELECTOR types.
;;
;; Within an interactive Emacs session, if `ert' is called with a regular
;; expression selector argument that matches to multiple test names, all
;; such tests are run, but only a summary line of output is displayed for
;; all passing tests.  Thus, the names of the passing matched tests are
;; not displayed.  Presently, within the *ert* output buffer, you must press
;; {j} on each individual test summary result to display and jump to the
;; test name and have its status spelled out.
;;
;; This library fixes the above with the `ert-results-toggle' command bound
;; to {t} within the ERT results buffer.  With each press, it toggles
;; between showing/hiding all test results.  After hiding the results,
;; point is left on the summary item associated with the test that had
;; been at point, if any.
;;
;; The second command, `ert-results-filter', is bound to {f} within
;; the ERT results buffer.  It filters the results to just those entries
;; whose status matches that of the current entry.  Point may be on a
;; results entry or on a result character in the results summary.
;;
;; With point on any of the statistics lines in the top section of the
;; results buffer, {f} does the following:
;;   Selector: - toggles showing/hiding all test results
;;   Passed:   - show passed tests only
;;   Failed:   - show failed tests only
;;   Skipped:  - show skipped tests only
;;   Total:    - show all tests
;;
;; This library also provides the following commands:
;;   `ert-results-hide'     - hide all test results
;;   `ert-results-show'     - show all test results
;;   `ert-results-display'  - show all test results; with prefix arg, hide
;;
;; To use:
;;
;;   Simply load some ERT tests, load this library, interactively run `ert'
;;   with a regular expression or string matching to some test names, wait
;;   for ERT to output its summary line in the results buffer and then
;;   press {t} to see all of the tests that were run together with their
;;   final status and any errors.  Press {t} again to hide all this detail.
;;   Press {f} to filter entries to the current context.

;;; Code:

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

(require 'ert)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(defvar ert--results-ewoc)
(defvar ert--results-progress-bar-button-begin)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun ert-results-display (&optional hide-flag status-to-show)
  "Show all test names and results in the *ert* buffer.
With optional prefix arg HIDE-FLAG non-nil, hide these instead.
With optional STATUS-TO-SHOW symbol (one of :passed :failed :skipped),
show/keep only entries that have that status."
  (interactive "P" ert-results-mode)
  (when (boundp 'ert--results-ewoc)
    (let ((ewoc ert--results-ewoc)
          (progress-bar-begin ert--results-progress-bar-button-begin)
          opoint
	  node
	  entry
	  entry-matches-status-to-show
	  result-num)
      (when (and hide-flag
		 (not (and (<= progress-bar-begin (point))
			   (< (point) (button-end (button-at progress-bar-begin))))))
	(setq result-num (ignore-errors
			   (ert--ewoc-position ewoc (ert--results-test-node-at-point)))))
      (save-excursion
	(goto-char progress-bar-begin)
	(while (and (<= progress-bar-begin (point))
		    (< (point) (button-end (button-at progress-bar-begin))))
	  (setq opoint (point)
		node (ewoc-nth ewoc (- (point) progress-bar-begin))
		entry (ewoc-data node))
	  ;; (when (xor hide-flag is-hidden)
	  ;;   (if hide-flag hide show)
	  ;; (xor hide-flag (or status-to-show (ert--ewoc-entry-hidden-p entry)))
	  (when status-to-show
	    (setq entry-matches-status-to-show
		  (ert-test-result-type-p
		   (ert-test-most-recent-result
		    (ert--ewoc-entry-test entry))
		   status-to-show)))
	  (setf (ert--ewoc-entry-hidden-p entry)
		(if entry-matches-status-to-show
		    nil
		  hide-flag))
	  (ewoc-invalidate ewoc node)
	  (goto-char (1+ opoint))))
      (when result-num
	(goto-char (+ progress-bar-begin result-num))))))

(defun ert-results-filter (&optional status-symbol)
  "Filter ert results entries to those matching optional STATUS-SYMBOL.
If STATUS-SYMBOL is null, use the entry type at point (context-sensitive).

If STATUS-SYMBOL is :passed :failed or :skipped (on any of the
Passed: Failed: or Skipped: header lines), only results with the
associated status are shown.

If STATUS-SYMBOL is :selector (on the header Selector: line), toggle
showing/hiding all test results.

If STATUS-SYMBOL is :total (on the header Total: line), toggle
showing/hiding all test results.

Return STATUS-SYMBOL."
  (interactive nil ert-results-mode)
  (when (setq status-symbol (or status-symbol (ert-results-filter-status-p)))
    (pcase status-symbol
      (:selector (ert-results-toggle))
      (:total (ert-results-show))
      ;; show only those test results matching the status at point
      (_ (ert-results-display t status-symbol)))
    status-symbol))

(defun ert-results-filter-status-p ()
  "Return the results status symbol for the current test entry, else return nil."
  (let ((status-symbol))
    (and (derived-mode-p 'ert-results-mode)
	 (or
	  ;; on a stats line
	  (save-excursion
	    (beginning-of-line)
	    (when (or (looking-at "\\(Passed\\|Failed\\|Skipped\\):[ \t]+[0-9]+$")
		      (looking-at "\\(Selector\\|Total\\):[ \t]"))
	      (setq status-symbol (intern-soft (concat ":" (downcase (match-string 1)))))))
	  ;; in or after a test result line
	  (setq status-symbol
		(save-excursion
		  (let* ((node (ert--results-test-node-or-null-at-point))
			 (entry (when node (ewoc-data node))))
		    (when entry
		      (ert-results--char-str-to-status-symbol
		       (char-to-string
			(ert-char-for-test-result
			 (ert-test-most-recent-result
			  (ert--ewoc-entry-test entry))
			 t)))))))
	  ;; At the start of a test result line, on the single
	  ;; character status Emacs push button that expands and
	  ;; collapses each result
	  (when (or (and (bolp) (button-at (point)))
		    ;; on the results summary line, on a single character status
		    (let ((progress-bar-begin ert--results-progress-bar-button-begin))
		      (and (<= progress-bar-begin (point))
			   (< (point) (button-end (button-at progress-bar-begin))))))
	  (setq status-symbol (ert-results--char-str-to-status-symbol (char-to-string (char-after)))))))
    status-symbol))

(defun ert-results-hide ()
  "Hide all test names and results in the *ert* buffer.
Leave point on the test summary result corresponding to the displayed
test it was on, if any."
  (interactive nil ert-results-mode)
  (ert-results-display t))

(defun ert-results-show ()
  "Show all test names and results in the *ert* buffer."
  (interactive nil ert-results-mode)
  (ert-results-display nil))

(defun ert-results-toggle ()
  "Toggle display of all test names and results in the *ert* buffer.
If any test is shown, then all are hidden; otherwise, all are displayed.
After a hide, leave point on the test summary result corresponding
to the displayed test it was on, if any."
  (interactive nil ert-results-mode)
  (if (save-excursion
	(goto-char (point-max))
	(ert--results-test-node-or-null-at-point))
      ;; some test is displayed, so hide all tests
      (ert-results-hide)
    (ert-results-show)))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun ert-results--char-str-to-status-symbol (char-str)
  "Given single CHAR-STR, convert it to a results status symbol for return."
  (pcase (downcase char-str)
    ("-" :unrun)
    ("." :passed)
    ("a" :aborted)
    ("f" :failed)
    ("p" :passed)
    ("q" :quit)
    ("s" :skipped)))

;;; ************************************************************************
;;; Key bindings
;;; ************************************************************************

(unless (lookup-key ert-results-mode-map "f")
  (define-key ert-results-mode-map "f" 'ert-results-filter))
(unless (lookup-key ert-results-mode-map "t")
  (define-key ert-results-mode-map "t" 'ert-results-toggle))

(provide 'ert-results)
;;; ert-results.el ends here
