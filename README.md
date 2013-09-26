Using Circular Programs for Higher-Order Syntax
===============================================

Implementats higher-order syntax for embedded languages with binding, as described in [Using Circular Programs for Higher-Order Syntax](http://dx.doi.org/10.1145/2500365.2500614) (Axelsson and Claessen, ICFP 2013).

The [Exp](Exp.hs) module implements the untyped lambda calculus as described in the paper.

[ExpSyntactic](ExpSyntactic.hs) and [ExpCompdata](ExpCompdata.hs) give generic implementations of binding that can be combined with other constructs.
