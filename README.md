circular-hos
============

Implementats higher-order syntax for embedded languages with binding, as described in Using Circular Programs for Higher-Order Syntax (Axelsson and Claessen, ICFP 2013).

The [`Exp`](Exp.hs) module implements the untyped lambda calculus as described in the paper.

[`ExpSyntactic`](ExpSyntactic.hs) and [`ExpCompdata`](ExpCompdata.hs) give generic implementations of binding that can be combined with other constructs.
