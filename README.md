# ert-results.el

This package augments `ert-results-mode` (where test results are
displayed) from the [Emacs Regression Test](https://www.gnu.org/software/emacs/manual/html_mono/ert.html) library, "ert.el", with four new commands:

{e} - `ert-results-edebug`  - edebugs the test at point in the results buffer or within a test definition

{f} - `ert-results-filter`  - filters display of test results based on point

`{M-x ert-results-run RET}` - runs the test at point in the results buffer or within a test definition; no keybinding, but could be bound to {r} by the user if desired to replace the built-in ERT command.

{t} - `ert-results-toggle`  - toggles display/hiding of all selected tests

## [Home Page](https://github.com/rswgnu/ert-results)

## Installation

Use the Melpa Emacs Lisp archive to install this as a regular package:

```lisp
(package-install 'ert-results)

(require 'ert-results)
```

or install it directly from the Github archive above as it is a single file code library.

## Usage

To use the package after installing it and requiring it:

1. Write and then load some ERT tests into your Emacs session.

2. Interactively run `ert` with a regular expression or string matching to
   some of your named test names

3. Wait for ERT to output its summary line with the test results.

4. Press {t} to see all of the tests that were run, together with their
   final status and any errors.

5. Press {t} again to hide all this detail.

6. Press {f} on any of the statistics header lines, on the results summary
   line or on any test result to filter to results with associated statuses.

7. On a test entry, press {r} to run any test, {e} to edebug a test or {d}
   to run a test with `debug-on-error' enabled, which generates a backtrace
   if any error is encountered during the run.

### Usage Details

Each press of {t}, the `ert-results-toggle` command, toggles between
showing/hiding all test results.  After hiding the results, point is left on
the summary item associated with the test that had been at point, if any.

Each press of {f}, the `ert-results-filter` command, filters the results to
just those test entries whose result status matches the current context at
point.  With point on a results entry or on a result character in the
results summary line, this filters to entries with that same result status.

With point on any of the statistics lines in the top section of the
results buffer, {f} does the following:

	| Context   | Command Executed                        |
	|-----------|-----------------------------------------|
	| Selector: | toggles showing/hiding all test results |
	| Passed:   | show passed tests only                  |
	| Failed:   | show failed tests only                  |
	| Skipped:  | show skipped tests only                 |
	| Total:    | show all tests                          |

A press of {e}, the `ert-results-edebug` command, edebugs the test at
point within the ERT results buffer, but also can be invoked with
`{M-x ert-results-edebug RET}` within any ert test definition in any
buffer.  It instruments the current version of the given test for
electric debugging (edebugging) and then steps through it in the test
source buffer with edebug.

`{M-x ert-results-run RET}' works just like `ert-results-edebug' but
instead of edebugging, it simply runs the current version of the test.
You can bind it yourself to {r} to replace ert's
`ert-results-rerun-test-at-point' command if you prefer a single
command that you can use in both the results buffer and test
definitions.

This package also provides the following commands:

    | Command               | Operation                                    |
	|-----------------------|----------------------------------------------|
	| `ert-results-hide`    | hide all test results                        |
	| `ert-results-show`    | show all test results                        |
	| `ert-results-display` | show all test results; with prefix arg, hide |

And the following functions for programming:

    | Function                      | Operation                               |
	|-------------------------------|-----------------------------------------|
    | `ert-results-all-test-bodies' | get a list of test bodies in the buffer |
    | `ert-results-all-test-names'  | get a list of test names in the buffer  |
    | `ert-results-all-test-tags'   | get a list of test tags in the buffer   |


## Rationale

See the Emacs documentation for [ert-select-tests](https://www.gnu.org/software/emacs/manual/html_mono/ert.html#Test-Selectors)
for test SELECTOR types.

Within an interactive Emacs session, if `ert` is called with a regular
expression selector argument that matches to multiple test names, all
such tests are run, but only a summary line of output is displayed for
all passing tests.  Thus, the names of the passing matched tests are
not displayed.  Presently, within the *ert* output buffer, you must press
{j} on each individual test summary result to display and jump to the
test name and have its status spelled out.

This package solves those problems by adding new context-sensitive
filter and toggle show/hide commands that give you easy control over
the views of your ERT test results.

This package could be merged as a standard part of ERT in GNU Emacs, as it
is wholly compatible, if an Emacs maintainer would do the work needed to
integrate it.
