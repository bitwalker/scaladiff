## What is it?

A Scala library for performing diffs over plain text. Results can be produced in raw, cleaned (human readable), or html formats. The base 
diff algorithm used is [Myer's diff algorithm](http://www.xmailserver.org/diff2.pdf).

## Current Status

The API is mostly firmed up, and it does produce accurate diffs (to the degree that my unit tests have verified this), 
in raw, cleaned, and html formats. It does not yet produce unidiff output, though I do plan on making that a feature. 
I would advise using this for experimental work, and report all bugs to me so that I can get them fixed up and wrap some
unit tests around them. I would like to get this stable and production ready as soon as possible, but I just don't feel
comfortable with the level of testing just yet.

## Roadmap

- Add unidiff output
- More tests
- Stress tests (how large can the source/modified text get before we're crawling)
- The ability to apply a diff as a patch to some text to produce the original version

## LICENSE

[Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0)
