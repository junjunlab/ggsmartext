# One-dimensional label-box de-overlap packing (a `smartAlign()` port)

`smart_align()` is an exact port of `smartAlign()` from circlize: given
one-dimensional intervals (label boxes) sorted by position and allowed
to overlap, it removes the overlaps by **iterative clustering and
packing**. Overlapping boxes are merged into a cluster, the cluster is
re-laid out inside its own span with the boxes placed end to end at
their original widths, and the cluster is then shifted back towards its
original midpoint, moving as little as possible while staying in range.
Iteration stops when the number of clusters no longer changes.

## Usage

``` r
smart_align(start, end, range = range(c(start, end)))
```

## Arguments

- start, end:

  Numeric vectors giving the left and right end of each label box, in
  any consistent unit (npc, data units, ...). The two vectors must have
  the same length. `start` must be sorted increasingly (an assumption of
  the algorithm, matching the reference implementation); for unsorted
  input call [`order()`](https://rdrr.io/r/base/order.html) first, or
  use
  [`box_arrange()`](https://junjunlab.github.io/ggutils/reference/box_arrange.md),
  which sorts by midpoint itself.

- range:

  Numeric vector of length 2, the range the boxes may occupy (e.g.
  `c(0, 1)`). Defaults to `range(c(start, end))`.

## Value

A data.frame with columns `start` and `end`, in the **same row order as
the input** (row i is the new position of the i-th input box).

## Details

The function is pure (it touches no environment and uses no random
numbers), so results are reproducible.

There are only two differences from the reference implementation, both
of them robustness improvements:

- an empty input returns a zero-row data.frame
  ([`circlize::smartAlign()`](https://rdrr.io/pkg/circlize/man/smartAlign.html)
  throws an error);

- invalid input (unequal lengths, non-finite values, a bad `range`)
  raises a clear error.

Note that when the total width of the boxes exceeds `diff(range)` the
overlaps cannot be removed: the cluster is centred (as in the reference
implementation) and the result **sticks out** of `range`. Callers should
check feasibility first
([`smart_stack()`](https://junjunlab.github.io/ggutils/reference/smart_stack.md)
drops low-priority labels before calling this function).

## See also

[`box_arrange()`](https://junjunlab.github.io/ggutils/reference/box_arrange.md)
for the other packing algorithm (BoxArrange from `ComplexHeatmap`),
which also accepts unsorted input;
[`smart_stack()`](https://junjunlab.github.io/ggutils/reference/smart_stack.md)
for the multi-row label engine.

## Examples

``` r
# two slightly overlapping boxes: packed end to end
smart_align(c(0.10, 0.12), c(0.14, 0.16), c(0, 1))
#>   start  end
#> 1  0.09 0.13
#> 2  0.13 0.17

# four identical boxes: spread around the original midpoint
smart_align(rep(0.5, 4), rep(0.6, 4), c(0, 1))
#>   start  end
#> 1  0.35 0.45
#> 2  0.45 0.55
#> 3  0.55 0.65
#> 4  0.65 0.75

# non-overlapping input is returned unchanged
smart_align(c(0.1, 0.5), c(0.2, 0.6), c(0, 1))
#>   start end
#> 1   0.1 0.2
#> 2   0.5 0.6
```
