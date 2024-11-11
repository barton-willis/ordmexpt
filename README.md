## Maxima function `ordmexpt`

This project aims to develop a new version of the Maxima function `ordmexpt`, 
which is a subroutine for Maxima's ordering predicate `great`. This 
rewrite attempts to clarify the logic of the function and to fix [bug #4383: 
great not transitive (so simplifya not idempotent)](https://sourceforge.net/p/maxima/bugs/4383/). Specifically, bug #4383 causes the following expression to not simplify correctly
~~~
(%i1)	(declare(z,complex), domain : complex)$
(%i2)	exp(z) + sqrt(exp(z)) + exp(z);
(%o2)	%e^z+sqrt(%e^z)+%e^z
~~~
Maxima's ordering predicate `great` is closely linked to simplification, 
so tweaking `great` can cause syntactic testsuite failures. To help 
distinguish syntactic from semantic failures, this project includes a 
modified function `approx-alike` that applies several transformations 
to both the expected and actual outputs. This modified function is *not* 
intended as a replacement to `approx-alike`. 

The last I tried, my alternative `ordmexpt` function runs the testsuite, including
the share testsuite, with seventy-eight unexpected failures. Of these failures, the alternative `approx-alike` determines that fifty-one of these failures are syntactic. Of the remaining twenty-seven failures, some are syntactic failures, others are failures such as a integral nounform when the result should be explicit. 
Here is a typical syntactic failure:
~~~
********************** Problem 80 (line 736) ***************
Input:
         - v
         ───
          2
specint(t    bessel_j(v, 2 sqrt(a) sqrt(t)) exp(- p t), t)


Result:
 v - 1   - a/p                               a
p      %e      v gamma_incomplete_lower(v, - ─)
                                             p
───────────────────────────────────────────────
                v  v/2
           (- 1)  a    gamma(v + 1)

This differed from the expected result:
 v - 1   - a/p                             - a
p      %e      v gamma_incomplete_lower(v, ───)
                                            p
───────────────────────────────────────────────
            v/2      v
           a    (- 1)  gamma(v + 1)

~~~
The file `rtest_great` test the function `great`. There is another test file (`rtest_shame`) of serious bugs that I have collected.

Running the testsuite calls `ordmexpt` about two million times, so we need to be
concerned with its efficiency. In addition to fixing bug #4383, the modified `ordmexpt` function fixes two testsuite failures-they are `rtest1`, #183 and `rtest_limit_extra` #259; the limit bug is  
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

... Which was correct, but was expected to be wrong due to a known bug in Maxima or SBCL.
~~~
This bug fix is *not* related to the extra simplifications in `approx-alike`.


