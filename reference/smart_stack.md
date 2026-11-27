# Multi-row label placement engine

`smart_stack()` assigns a set of label boxes (each with an axis position
`positions` and a width `widths`, both in npc) to one of `n` rows (`n`
columns for a y axis) so that every label can be drawn, labels within a
row do not overlap, and each label stays as close as possible to its
original position. It is the placement engine behind
[`guide_axis_smart()`](https://junjunlab.github.io/ggutils/reference/guide_axis_smart.md)
and can also be used on its own.

## Usage

``` r
smart_stack(
  positions,
  widths,
  n,
  range = c(0, 1),
  extend = 0,
  padding = 0,
  priority = NULL
)
```

## Arguments

- positions:

  Numeric vector of axis positions (npc, 0..1) of the label boxes. Does
  not need to be sorted.

- widths:

  Numeric vector of box widths (npc), the same length as `positions`.

- n:

  Positive integer, the number of rows (columns for a y axis). It also
  sets the total capacity.

- range:

  Numeric vector of length 2, the drawable range of the axis, default
  `c(0, 1)`.

- extend:

  Non-negative scalar, how far labels may stick out of `range` (`0.02`
  widens the axis span by 0.02 on each side); same meaning as the
  argument of the same name in
  [`guide_axis_smart()`](https://junjunlab.github.io/ggutils/reference/guide_axis_smart.md).

- padding:

  Non-negative scalar, the minimum gap required between neighbouring
  boxes in the same row, in the same unit as `positions` (npc).

- priority:

  `NULL` (default, all labels equally important) or a numeric vector the
  same length as `positions`. **Larger values are kept first**; when
  space runs out the low-priority labels are dropped.

## Value

A data.frame with one row per input box, in input order, with columns:

- `index`: the input index (`1..length(positions)`);

- `row`: the assigned row (`1..n`); `NA` for dropped labels;

- `start`, `end`: the left and right end of the placed box (npc);

- `center`: the centre of the placed box (npc);

- `shown`: logical, whether the label should be drawn.

## Details

The algorithm has three steps:

1.  **Usable range**: `range` is widened on both sides by
    `extend * diff(range)` to give `lim`; boxes wider than `diff(lim)`
    can never fit and are dropped.

2.  **Row assignment**: boxes are processed from the highest to the
    lowest priority (larger `priority` first; ties by position, then by
    index). Each box goes to the row that currently carries the least
    load among the rows that still have capacity (load = sum of widths
    in the row plus the `padding` gaps it needs), so with equal
    priorities the labels alternate between rows along the axis
    (water-filling balance). Labels that fit in no row are dropped
    (`shown = FALSE`).

3.  **Packing inside a row**: within a row the boxes keep their axis
    order by centre position; each box is inflated by `padding / 2` on
    both sides, the packing algorithm removes the overlaps, and the
    boxes are shrunk back, so neighbouring labels in a row end up
    exactly `padding` apart. An order-preserving projection clamps the
    centres into the usable range before inflation, so the result never
    leaves `lim`.

- Every box with `shown = TRUE` satisfies
  `lim[1] <= start < end <= lim[2]`; within a row, after sorting by
  `start`, `start[i+1] >= end[i] + padding` (equality holds when no edge
  clamping kicks in).

- The order inside a row follows the box **centres**, i.e. the axis
  order.

- Row assignment depends on priority and load balancing only and is
  **deterministic**: the same input always gives the same output, with
  no random numbers involved.

- The function guarantees "drawn means not overlapping", but it never
  squeezes a label below its `widths`; labels that do not fit are
  dropped (`shown = FALSE`).

- When there are many labels and the `n` rows do not provide enough
  capacity, the lowest-priority labels are dropped first; among equal
  priorities the labels nearest the left end of the axis are processed
  first.

## See also

[`smart_align()`](https://junjunlab.github.io/ggutils/reference/smart_align.md),
[`box_arrange()`](https://junjunlab.github.io/ggutils/reference/box_arrange.md)

## Examples

``` r
# six labels spread along the axis in 2 rows: alternate, barely moved
pos <- seq(0.1, 0.9, length.out = 6)
smart_stack(pos, widths = rep(0.08, 6), n = 2)
#>   index row start  end center shown
#> 1     1   1  0.06 0.14   0.10  TRUE
#> 2     2   2  0.22 0.30   0.26  TRUE
#> 3     3   1  0.38 0.46   0.42  TRUE
#> 4     4   2  0.54 0.62   0.58  TRUE
#> 5     5   1  0.70 0.78   0.74  TRUE
#> 6     6   2  0.86 0.94   0.90  TRUE

# three labels crowded around 0.5 still do not overlap in 3 rows
smart_stack(c(0.48, 0.5, 0.52), widths = rep(0.2, 3), n = 3)
#>   index row start  end center shown
#> 1     1   1  0.38 0.58   0.48  TRUE
#> 2     2   2  0.40 0.60   0.50  TRUE
#> 3     3   3  0.42 0.62   0.52  TRUE

# when space runs out, priority decides what is dropped
smart_stack(
  positions = rep(0.5, 5), widths = rep(0.4, 5), n = 1,
  priority = c(1, 5, 3, 4, 2)
)
#>   index row start end center shown
#> 1     1  NA   0.3 0.7    0.5 FALSE
#> 2     2   1   0.1 0.5    0.3  TRUE
#> 3     3  NA   0.3 0.7    0.5 FALSE
#> 4     4   1   0.5 0.9    0.7  TRUE
#> 5     5  NA   0.3 0.7    0.5 FALSE
```
