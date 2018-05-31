# Elasmobranch

## Using Elasmobranch in your VSTS project

The easy way would be to run this command in one of your VSTS Linux
build scripts.

```shell
curl https://raw.githubusercontent.com/tomjaguarpaw/elasmobranch/release-0/run-elasmobranch-in-vsts.sh | sh
```

However, that would count as running untrusted code.  Instead you
should make sure that you audit that script and take a checksum to
make sure it never changes.  I suggest using following code which
verifies the checksum.

```shell
die() {
    MESSAGE=$1
    echo $MESSAGE
    exit 1
}

verified_download() {
    URL="$1"
    OUTPUT="$2"
    CHECKSUM="$3"

    curl -o "$OUTPUT" "$URL"
    echo "$CHECKSUM  $OUTPUT" | sha256sum --strict --check --status || die "Hash of $OUTPUT was wrong!"
}

verified_download 'https://raw.githubusercontent.com/tomjaguarpaw/elasmobranch/release-0/run-elasmobranch-in-vsts.sh' run-elasmobranch-in-vsts.sh b8ca79ea204da1ba3b3f283e402425ec21a2da94184bb4cbb914feb734a46c10
sh ./run-elasmobranch-in-vsts.sh
```

(The lines are very long.  Make sure they don't wrap incorrectly when
pasting them into your editor.)  That's much more verbose but good
security practice.

If you don't already have a Linux build script into which to put this
code then you can create a new VSTS task.  All you need to do is find
some means of running the above script.  For example you could

* Click the "+" on one of your build's "phases"
* Click the "Utility" tab
* Hover over "Command Line" and "Add" it
* In the new "Run" task choose "tool" to be `sh` and "arguments" to be
  `vsts.sh`
* Then put the code above into a file called `vsts.sh` in the
  top-level directory of your repo

Your Elasmobranch report will appear amongst your build artifacts.  To
find it:

* Go to the status page of your build
* Click "Build <build number>" on the left hand side
* Click the "Artifacts" link (next to "Summary", "Timeline", ...)
* "Explore" the "Elasmobranch" folder
* Click the arrow next to the "Elasmobranch" folder entry
* Click "..." on the right hand  side of "elasmobranch.html"
* Then you can "Download" or "Copy download url"

## Known issues

If you have any difficulties with Elasmobranch or have any questions
or comments about it then please [open a new
ticket](https://github.com/tomjaguarpaw/elasmobranch/issues/new).  The
following issues are known:

* It takes a long time to click through to the report.  It would
  probably be nice to provide direct links to all your Elasmobranch
  reports on some web page hosted on Azure.

* It's unnecessarily slow.  `elasmobranch` compares every pair of
  branches in every run.  This takes time *O(n^2)* in the number of
  branches.  That's probably OK because it probably won't be run too
  frequently, but with caching we could speed it up to *O(n)*.  If
  you're affected by this issue then [open a new
  ticket](https://github.com/tomjaguarpaw/elasmobranch/issues/new).

* The report has "Submit another repo" links that don't (and
  shouldn't) work.  They'll be removed in a future version.

* There is no key for the colours in the table cells.

* There are no tooltips for the table cells.

* The three-letter abbreviations don't work well with username-prefixed
  branches.
