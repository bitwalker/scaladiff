# What is it?

A library of algorithms that performs diff, fuzzy matching, and patch functions over plain text.
It is based on the (google-diff-match-patch)[http://code.google.com/p/google-diff-match-patch/] library,
and shares the same license (Apache License 2.0).

The base diff algorithm used is (Myer's diff algorithm)[http://simplygenius.net/Article/DiffTutorial1],
which is generally considered to be the best general purpose diff. A series of pre-diff speedups and post-diff
cleanups is also applied to improve both performance and diff quality.

The base matching algorithm used is the (Bitap algorithm)[https://en.wikipedia.org/wiki/Bitap_algorithm].

# LICENSE

(Apache License 2.0)[http://www.apache.org/licenses/LICENSE-2.0]
