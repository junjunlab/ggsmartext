# Package index

## Smart axis guides

When a discrete axis carries too many labels, keep only the ones you ask
for, draw a leader line back to each tick, and spread the labels over
several rows (x axis) or columns (y axis) outside the panel.

- [`guide_axis_smart()`](https://junjunlab.github.io/ggutils/reference/guide_axis_smart.md)
  : Smart axis labels for ggplot2

## Coordinates

Bend an ordinary ggplot into an Archimedean spiral: x runs along the
spiral as the angle, y as the radius of the lane, with an angular axis
and a radial tick axis, and full facet_wrap() / facet_grid() support.

- [`coord_spiral()`](https://junjunlab.github.io/ggutils/reference/coord_spiral.md)
  : Spiral coordinates for ggplot2
- [`scale_x_spiral_axis()`](https://junjunlab.github.io/ggutils/reference/scale_spiral_axis.md)
  [`scale_y_spiral_axis()`](https://junjunlab.github.io/ggutils/reference/scale_spiral_axis.md)
  : Fine-tune the axes drawn along a spiral

## Cluster trees

Add a hierarchical cluster tree to a discrete x / y axis: the matching
scales reorder the categories by dendrogram leaf order so tree and data
stay aligned, and faceted plots get one tree per panel attached inside
the strip.

- [`guide_axis_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md)
  [`scale_x_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md)
  [`scale_y_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md)
  : Cluster axis guide for ggplot2
- [`cluster_order()`](https://junjunlab.github.io/ggutils/reference/cluster_order.md)
  : Retrieve the dendrogram leaf order
- [`strip_cluster()`](https://junjunlab.github.io/ggutils/reference/strip_cluster.md)
  : Keep cluster trees inside the facet strip

## Facets

A facet_grid that frees each panel: scales = “free_x” / “free_y” /
“free” are released per panel instead of per column or per row as in
facet_grid().

- [`facet_gridn()`](https://junjunlab.github.io/ggutils/reference/facet_gridn.md)
  [`FacetGridN`](https://junjunlab.github.io/ggutils/reference/facet_gridn.md)
  :

  A `facet_grid()` that can free scales panel by panel

## Layout algorithms

The three low level placement algorithms used inside the guides: one
dimensional anti overlap packing (a port of circlize::smartAlign), box
arrangement (a port of ComplexHeatmap::BoxArrange) and multi row
stacking on top of them. They can also be called on their own.

- [`smart_align()`](https://junjunlab.github.io/ggutils/reference/smart_align.md)
  : 一维标签盒防重叠压紧算法（smartAlign 移植）
- [`box_arrange()`](https://junjunlab.github.io/ggutils/reference/box_arrange.md)
  : 一维标签盒聚类排布算法（BoxArrange 移植）
- [`smart_stack()`](https://junjunlab.github.io/ggutils/reference/smart_stack.md)
  : 多行标签排布引擎

## Datasets

Real data used by the examples and the vignette: an RNA seq heat map
from GEO GSE52778 (a 300 highly variable genes x 8 samples matrix of row
z scores) and the marker table and dot plot data from the Seurat PBMC3k
tutorial.

- [`airway_heatmap`](https://junjunlab.github.io/ggutils/reference/airway_heatmap.md)
  : Processed RNA-seq expression matrix (GEO GSE52778)
- [`pbmc3k_markers`](https://junjunlab.github.io/ggutils/reference/pbmc3k_markers.md)
  : PBMC3k single-cell marker table
- [`pbmc3k_dotplot`](https://junjunlab.github.io/ggutils/reference/pbmc3k_dotplot.md)
  : PBMC3k dot plot data
