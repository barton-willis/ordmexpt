## Maxima function `ordmexpt`

This project aims to develop a new version of the Maxima function `ordmexpt`, which serves as a subroutine for Maxima's ordering predicate `great`. This rewrite specifically addresses [bug #4383: great not transitive (so simplifya not idempotent)](https://sourceforge.net/p/maxima/bugs/4383/), with the goal of fixing this bug and simplifying the code.

Maxima's ordering predicate `great` is closely linked to simplification, so tweaking `great` can cause syntactic testsuite failures. To help distinguish syntactic from semantic failures, this project includes a modified function `approx-alike` that applies several transformations to both the expected and actual outputs.

The last I tried, my alternative `ordmexpt` function runs the testsuite with 54 unexpected failures. Of these failures,
the alternative `approx-alike` determines that twenty of these failures are syntactic. Of the remaining, 34 failures,
many are syntactic.




