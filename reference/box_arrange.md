# One-dimensional label-box clustering and packing (a `BoxArrange` port)

`box_arrange()` re-implements as pure functions the R5 reference class
`BoxArrange` (plus its helpers `Box` and `BoxCluster`) from
ComplexHeatmap:

## Usage

``` r
box_arrange(start, end, range = range(c(start, end)))
```

## Arguments

- start, end:

  Numeric vectors giving the left and right end of each label box; they
  must have the same length. `start` need not be sorted.

- range:

  Numeric vector of length 2, the range the boxes may occupy (e.g.
  `c(0, 1)`). Defaults to `range(c(start, end))`.

## Value

A data.frame with columns `start` and `end`, in the **same row order as
the input** (row i is the new position of the i-th input box).

## Details

1.  sort the boxes by **midpoint** (the input need not be sorted);

2.  merge overlapping boxes (touching ends count) into clusters, in
    midpoint order;

3.  for each cluster use `sum(width)` as its span, lay the boxes end to
    end inside `range` (`flatten`), then iteratively merge the overlaps
    created by the edge shifts (`merge`);

4.  shift clusters that fall outside back into `range`
    (`adjust_to_range`) and merge once more;

5.  restore the original input order.

When the total width exceeds `diff(range)` (the boxes simply do not fit)
it falls back to the **proportional spread** of
`ComplexHeatmap:::smartAlign2()`: the first and last box stay glued to
the ends and the rest are spread proportionally, so overlaps remain but
every box stays inside `range`.

On feasible input the result matches the reference implementation
(`ComplexHeatmap:::smartAlign2()`, the wrapper around `BoxArrange`)
element by element within 1e-10. The differences are robustness and
reproducibility improvements:

- an empty input returns a zero-row data.frame; the reference throws
  `subscript out of bounds`;

- boxes with identical midpoints are ordered **deterministically** (the
  reference uses `rank(ties.method = "random")`, which consumes random
  numbers and is not reproducible); results can only differ from the
  reference when exact ties occur;

- a single box wider than `range` is centred; the reference returns
  `-Inf`.

The reference returns a matrix on the feasible branch and a data.frame
on the infeasible one; this function always returns a data.frame.

## See also

[`smart_align()`](https://junjunlab.github.io/ggutils/reference/smart_align.md)
for the circlize packing algorithm (which requires sorted input);
[`smart_stack()`](https://junjunlab.github.io/ggutils/reference/smart_stack.md)
for the multi-row label engine.

## Examples

``` r
# two slightly overlapping boxes are packed end to end
box_arrange(c(0.10, 0.12), c(0.14, 0.16), c(0, 1))
#>   start  end
#> 1  0.09 0.13
#> 2  0.13 0.17

# unsorted input is fine: sorted by midpoint, returned in input order
box_arrange(c(0.60, 0.10, 0.30), c(0.64, 0.14, 0.34), c(0, 1))
#>   start  end
#> 1   0.6 0.64
#> 2   0.1 0.14
#> 3   0.3 0.34

# when the widths exceed the range it falls back to a proportional spread
box_arrange(c(0.1, 0.2, 0.3), c(0.6, 0.7, 0.8), c(0, 1))
#>   start  end
#> 1  0.00 0.50
#> 2  0.25 0.75
#> 3  0.50 1.00
```
