# My First Parser

[Newick format](https://en.wikipedia.org/wiki/Newick_format) is a concise textual way of representing weighted trees.  For example,
```
(a:1,b:2);
```
represents a tree with an unnamed root node and two leaves `a` and `b`,
where the edge from the root to `a` has weight `1` and the edge from the
root to `b` has weight `2`, and
```
((a:1,b:2):3,c:4,(d:5,e:6):7);
```
is a tree with an unnamed root node that has three subtrees hanging from
it: one is the tree we just saw, and the weight of the edge from the
root to the subtree root is 3. The second subtree is a single leaf called `c`, and
the edge from the root to `c` has weight 3.  The last subtree has an
unnamed root node and two leaves `d` and `e`. The weight of the edge
from the main root to the subtree root is `7`, and the edges from the
subtree root to `d` and `e` have weights `5` and `6`.

This Haskell module contains a simple `WeightedTree` data type and a
recursive parser written using [Parsec](http://hackage.haskell.org/package/parsec)
that parses Newick format into WeightedTrees. To use it interactively in
`ghci`, try

```
par "((a:1,b:2):3,c:4,d:5);"
```
The result is wrapped in an `Either`; you can extract it with `rights`.
