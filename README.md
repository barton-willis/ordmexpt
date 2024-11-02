## Maxima function `ordmexpt`

This project aims to develop a new version of the Maxima function `ordmexpt`, 
which serves as a subroutine for Maxima's ordering predicate `great`. This 
rewrite attempts clarify the logic of the function and to repair [bug #4383: 
great not transitive (so simplifya not idempotent)](https://sourceforge.net/p/maxima/bugs/4383/).

Maxima's ordering predicate `great` is closely linked to simplification, 
so tweaking `great` can cause syntactic testsuite failures. To help 
distinguish syntactic from semantic failures, this project includes a 
modified function `approx-alike` that applies several transformations 
to both the expected and actual outputs. This modified function is *not* 
intended as a replacement to `approx-alike`. 

The last I tried, my alternative `ordmexpt` function runs the testsuite 
with 54 unexpected failures. Of these failures, the alternative `approx-alike` 
determines that twenty of these failures are syntactic. Of the remaining
34 failures, many are syntactic. 

Running the testsuite calls `ordmexpt` about two million times. In addition to
fixing bug #4383, the modified `ordmexpt` function fixes two other bugs; one
of these is 
~~~
Running tests in rtest_limit_extra:
********************** Problem 259 (line 909) ***************
Input:
                 - a
                 ───
              a   2
limit((%i + 1)  2   , a, inf)


Result:
ind

... Which was correct, but was expected to be wrong due to a known bug in
 Maxima or SBCL.
~~~



