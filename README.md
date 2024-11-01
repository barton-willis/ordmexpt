## Maxima function `ordmexpt`

This is an attempt to write a new version of the Maxima function 'ordmexpt'. This function is a subroutine of Maxima's ordering predicate `great`. In particular, this rewrite attempts to
fix the bug #4383 great not transitive (so simplifya not idempotent).
