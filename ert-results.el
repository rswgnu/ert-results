;;; ert-results.el --- Filter ERT test results display   -*- lexical-binding: t; -*-
;; Usage:        GNU Emacs Lisp Library
;; Keywords:     lisp, maint, tools
;; Version:      1.0.6
;;
;; Author:       Robert Weiner <rsw@gnu.org>
;;
;; Package-Requires: ((emacs "24.1"))
;; URL:              https://github.com/rswgnu/ert-results
;;
;; Orig-Date:    28-Dec-23 at 14:52:30
;; Last-Mod:      8-Jan-24 at 13:58:02 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2023  Free Software Foundation, Inc.
;;
;; Licensed under the GNU General Public License, version 3.
;;
;; This file is not yet part of Emacs.  It requires Emacs 24.1 or above.
;;
;;; Commentary:
;;
;; See documentation for `ert-select-tests' for test SELECTOR types.
;;
;; Within an interactive Emacs session, if `ert' is called with a
;; regular expression selector argument that matches to multiple test
;; names, all such tests are run, but only a summary line of output is
;; displayed for all passing tests.  Thus, the names of the passing
;; matched tests are not displayed.  Presently, within the *ert* output
;; buffer, you must press {j} on each individual test summary result to
;; display and jump to the test name and have its status spelled out,
;; rather than abbreviated.  This library fixes these problems and
;; extends the ert library with a number of useful commands.
;;
;; The first command, `ert-results-toggle', is bound to {t} within the
;; ERT results buffer.  With each press, it toggles between showing and
;; hiding all test results.  After hiding the results, point is left on
;; the summary item associated with the test that had been at point, if
;; any.
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
;; The third command, `ert-results-edebug', is bound to {e} within the
;; ERT results buffer, but also can be invoked by name within any ert
;; test definition in any buffer.  It instruments the current version of
;; the given test for electric debugging (edebugging) and then steps
;; through it in the test source buffer with edebug.
;;
;; The fourth command, `ert-results-run', is not bound to a key.  It
;; works just like `ert-results-edebug' but instead of edebugging, it
;; simply runs the current version of the test.  You can bind it
;; yourself to {r} to replace ert's `ert-results-rerun-test-at-point'
;; command if you prefer a single command that you can use in both the
;; results buffer and test definitions.
;;
;; All key bindings for this library are made only if the "ert" library
;; or the user has not already bound them in `ert-results-mode'.
;;
;; ------------
;;
;; This library also provides the following commands for the results buffer:
;;   `ert-results-hide'     - hide all test results
;;   `ert-results-show'     - show all test results
;;   `ert-results-display'  - show all test results; with prefix arg, hide
;;
;; This library also provides these functions that map over all tests
;; in the results buffer:
;;   `ert-results-all-test-bodies' - get a list of test bodies in the buffer
;;   `ert-results-all-test-names'  - get a list of test names in the buffer
;;   `ert-results-all-test-tags'   - get a list of test tags in the buffer
;;
;; ------------
;;
;; To use:
;;
;;   Simply load some ERT tests, load this library, interactively run `ert'
;;   with a regular expression or string matching to some test names, wait
;;   for ERT to output its summary line in the results buffer and then
;;   press {t} to see all of the tests that were run together with their
;;   final status and any errors.  Press {t} again to hide all this detail.
;;   Press {f} to filter entries to the current context and {e} to edebug
;;   a test at point.

;;; Code:

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

(require 'ert)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(defvar ert-results--buffer)
(defvar ert--results-ewoc)
(defvar ert--results-progress-bar-button-begin)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun ert-results-display (&optional hide-flag status-to-show)
  "Show all test names and results in the *ert* buffer.
With optional prefix arg HIDE-FLAG non-nil, hide these instead.
With optional STATUS-TO-SHOW symbol (one of :passed :failed
:aborted :skipped), show/keep only entries with that status."
  (interactive "P" ert-results-mode)
  (unless (derived-mode-p 'ert-results-mode)
    (error "(ert-results-display): Use only in an ERT results buffer"))
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

(defun ert-results-edebug ()
  "Eval and edebug any ert test at point and return t, else return nil.
Point may be within an ert test def or in an ert results buffer and on a
test or test summary item."
  (interactive)
  (ert-results--run-test-at-definition t))

(defun ert-results-filter (&optional status-symbol)
  "Filter ert results entries to those matching optional STATUS-SYMBOL.
If STATUS-SYMBOL is null, use the entry type at point (context-sensitive).

If STATUS-SYMBOL is :passed :failed :aborted or :skipped (on any of the
Passed: Failed: Aborted or Skipped: header lines), only results with the
associated status are shown.

If STATUS-SYMBOL is :selector (on the header Selector: line), toggle
showing/hiding all test results.

If STATUS-SYMBOL is :total (on the header Total: line), toggle
showing/hiding all test results.

If STATUS-SYMBOL is :aborted (on the header Aborted line), do nothing.

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
		      (looking-at "\\(Selector\\|Total\\):[ \t]")
		      (looking-at "\\(Aborted\\)\\."))
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

(defun ert-results-run ()
  "Eval and run any ert test at point and return t, else return nil.
Point may be within an ert test def or in an ert results buffer and on a
test or test summary item."
  (interactive)
  (ert-results--run-test-at-definition))

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

(defun ert-results-all-test-bodies ()
  "Return a list of the body of each test in a results buffer."
  (ert-results--map-tests #'ert-test-body))

(defun ert-results-all-test-names ()
  "Return a list of the symbol for each test in a results buffer."
  (ert-results--map-tests #'ert-test-name))

(defun ert-results-all-test-tags ()
  "Return a list of the tags associated with each test in a results buffer."
  (ert-results--map-tests #'ert-test-tags))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun ert-results--all-test-numbers-and-names ()
  "Return a list of (test-num . test-symbol) items from the ert results buffer.
Test numbers start at 0."
  (let ((counter -1)
	(results-buffer (get-buffer ert-results--buffer)))
    (when results-buffer
      (with-current-buffer results-buffer
	(ert-results--map-tests
	 (lambda (test)
	   (setq counter (1+ counter))
	   (cons counter (ert-test-name test))))))))

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

(defun ert-results--goto-test-number (test-num)
  "Jump to the ert results buffer summary item given by TEST-NUM.
Trigger an error if cannot jump there."
  (if (not (wholenump test-num))
      (error "(ert-results--goto-test-number): `test-num' must be a whole number, not '%s'"
	     test-num)
    (let ((results-buffer (get-buffer ert-results--buffer)))
      (if (not results-buffer)
	  (error "(ert-results--goto-test-number): No results \"%s\" buffer found"
		 ert-results--buffer)
	(switch-to-buffer results-buffer)
	(goto-char (+ ert--results-progress-bar-button-begin test-num))))))

(defun ert-results--in-summary-bar-p (&optional pos)
  "Return t if optional POS (default = point) is in the ert results status bar.
Return nil otherwise."
  (when (derived-mode-p 'ert-results-mode)
    (unless pos
      (setq pos (point)))
    (let ((progress-bar-begin ert--results-progress-bar-button-begin))
      (and (<= progress-bar-begin pos)
	   (< pos (button-end (button-at progress-bar-begin)))))))

(defun ert-results--map-tests (func)
  "In an ert results buffer, return result of applying FUNC over its tests.
For example, to get all test names:
  (ert-results-map-tests #\\='ert-test-name)"
  (unless (derived-mode-p 'ert-results-mode)
    (error "(ert-results-map-tests): Use only in an ERT results buffer"))
  (let ((ewoc ert--results-ewoc)
	node
	entry
	result
	results)
    (ewoc--set-buffer-bind-dll ewoc
      (setq node (ewoc-nth ewoc 0))
      (while (and node (setq entry (ewoc-data node))
		  (ert--ewoc-entry-p entry))
	(setq result (funcall func (ert--ewoc-entry-test entry))
	      results (cons result results)
	      node (ewoc--node-next dll node)))
      results)))

(defun ert-results--run-test-at-definition (&optional edebug-it)
  "Eval and run any ert test at point and return t, else return nil.
Point may be within an ert test def or in an ert results buffer and on a
test or test summary item.

With optional EDEBUG-IT non-nil, edebug the test when it is run."
  (save-excursion
    (save-window-excursion
      (let ((in-summary-bar (ert-results--in-summary-bar-p)))
	(when (derived-mode-p 'ert-results-mode)
	  ;; Next line errors if not found
	  (ert-results-find-test-at-point-other-window))
	;; Ensure run the latest version of the test, either with the
	;; edebugger if `edebug-it' is non-nil; otherwise, with the
	;; normal evaluator.
	(let ((test-sym (if edebug-it
			    (edebug-defun)
			  (eval-defun nil))))
	  (when (and test-sym (ert-test-boundp test-sym))
	    (let ((test-num (ert-results--test-number test-sym)))
	      (if test-num
		  ;; test is already in results buffer
		  (progn (ert-results--goto-test-number test-num)
			 (if in-summary-bar
			     (ert-results-rerun-test-at-point)
			   (ert-results-jump-between-summary-and-result)
			   (ert-results-rerun-test-at-point)
			   (goto-char (+ (point) 2))))
		(ert test-sym))
	      t)))))))

(defun ert-results--test-number (test-symbol)
  "Return the index number of the Ert results buffer TEST-SYMBOL.
Indexes start from 0."
  (when (and test-symbol (symbolp test-symbol) (ert-test-boundp test-symbol))
    (car (rassq test-symbol (ert-results--all-test-numbers-and-names)))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar ert-results--buffer "*ert*"
  "Buffer name for standard ERT results.")

;;; ************************************************************************
;;; Key bindings
;;; ************************************************************************

(unless (lookup-key ert-results-mode-map "e")
  (define-key ert-results-mode-map "e" 'ert-results-edebug))
(unless (lookup-key ert-results-mode-map "f")
  (define-key ert-results-mode-map "f" 'ert-results-filter))
(unless (lookup-key ert-results-mode-map "t")
  (define-key ert-results-mode-map "t" 'ert-results-toggle))

(provide 'ert-results)
;;; ert-results.el ends here
