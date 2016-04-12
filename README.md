## What is it?

A Scala library for performing diffs over plain text. Results can be produced in raw, cleaned (human readable), or html formats. The base 
diff algorithm used is [Myer's diff algorithm](http://www.xmailserver.org/diff2.pdf).

## Current Status

The API is mostly firmed up, and it does produce accurate diffs (to the degree that my unit tests have verified this), 
in raw, cleaned, and html formats. It does not yet produce unidiff output, though I do plan on making that a feature. 
I would advise using this for experimental work, and report all bugs to me so that I can get them fixed up and wrap some
unit tests around them. I would like to get this stable and production ready as soon as possible, but I just don't feel
comfortable with the level of testing just yet.

## Installation

Add the following to your Build.scala file:

```
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
    ... other dependencies ...
    "net.ironforged" % "scaladiff_2.10" % "0.1-SNAPSHOT"
)
```

## Usage

Import `net.ironforged.scaladiff.Diff` and create a Diff instance with `val diff = Diff.create(originalText, modifiedText)`. 
You can then access the diff text using one of three variations:

- `diff.toString` is the raw diff (ex. `bills +s+w-bo-ards`)
- `diff.humanized` is the more readable version of the diff (ex. `bills -[boa]+[swo]rds`)
- `diff.html` is the html version of the humanized diff for display on a web page

Unidiff output will be added soon!

## Roadmap

- Add unidiff output
- More tests
- Stress tests (how large can the source/modified text get before we're crawling)
- The ability to apply a diff as a patch to some text to produce the original version

## LICENSE

[Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0)
