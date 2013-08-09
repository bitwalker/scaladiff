## What is it?

A Scala library for performing diffs over plain text. Results can be produced in raw, cleaned (human readable), or html formats. The base 
diff algorithm used is [Myer's diff algorithm](http://www.xmailserver.org/diff2.pdf).

## Current Status

Not stable at all, and fluctuates between working and not working as bugs are tackled, fixed, and uncovered.
I would not use this yet, however the API is beginning to firm up, and it does produce usable diffs, in raw, cleaned,
and html formats. It does not yet produce unidiff output, though I do plan on making that a feature. The diffs
are relatively untested, which is why I say it fluctuates between working states - it works until I find the next
bug essentially.

## Roadmap

- Wrap tests around current features
- Add unidiff output
- More tests
- Stress tests
- The ability to apply a diff as a patch to some text to produce the original version

## LICENSE

[Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0)
