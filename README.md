# ert-results.el

This package augments `ert-results-mode` (where test results are
displayed) from the [Emacs Regression Test](https://www.gnu.org/software/emacs/manual/html_mono/ert.html) library, "ert.el", with two new commands:

{f} - (ert-results-filter) - filters display of test results based on point

{t} - (ert-results-toggle) - toggles display/hiding of all selected tests

## [Home Page](https://github.com/rswgnu/ert-results)

## Installation

Use the Melpa Emacs Lisp archive to install this as a regular package:

```lisp
(package-install 'ert-results)

(require 'ert-results)
```

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

This package also provides the following commands:

    | Command               | Operation                                  |
	|-----------------------|--------------------------------------------|
	| `ert-results-hide`    |hide all test results                       |
	| `ert-results-show`    |show all test results                       |
	| `ert-results-display` |show all test results; with prefix arg, hide|


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
