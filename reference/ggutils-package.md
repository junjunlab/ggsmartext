# ggutils: Smart Axis Labels for ggplot2

When a discrete axis of a `ggplot2` plot carries more category labels
than it can display,
[`guide_axis_smart()`](https://junjunlab.github.io/ggutils/reference/guide_axis_smart.md)
shows only the labels you ask for, connects every displayed label to its
tick with a leader line and spreads the labels over several rows (or
columns) so that the available page space is used efficiently.

## Details

The one-dimensional arrangement is a port of the `smartAlign` algorithm
used by the [circlize](https://github.com/jokergoo/circlize) package,
and of the box arrangement used by the
[ComplexHeatmap](https://github.com/jokergoo/ComplexHeatmap) package.

## Main functions

- [`guide_axis_smart()`](https://junjunlab.github.io/ggutils/reference/guide_axis_smart.md)
  – the smart axis guide.

- [`smart_align()`](https://junjunlab.github.io/ggutils/reference/smart_align.md)
  – one-dimensional, overlap-free arrangement of labelled boxes (pure
  `smartAlign` port).

- [`box_arrange()`](https://junjunlab.github.io/ggutils/reference/box_arrange.md)
  – cluster based arrangement of labelled boxes.

- [`smart_stack()`](https://junjunlab.github.io/ggutils/reference/smart_stack.md)
  – multi-row arrangement of labels along one axis.

## See also

Useful links:

- <https://github.com/junjunlab/ggutils>

- <https://junjunlab.github.io/ggutils/>

- Report bugs at <https://github.com/junjunlab/ggutils/issues>

## Author

**Maintainer**: Zhang Jun <1138976957@qq.com>
