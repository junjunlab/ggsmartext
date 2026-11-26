# Package index

## 智能轴标签 (Smart axis guides)

分类轴上的标签过多时，只显示用户指定的那一部分标签，
用引导线把每个标签连回它对应的刻度，并把标签在轴外侧 分行（x
轴）或分列（y 轴）错开排布。

- [`guide_axis_smart()`](https://junjunlab.github.io/ggutils/reference/guide_axis_smart.md)
  : Smart axis labels for ggplot2

## 坐标系统 (Coordinates)

把普通 ggplot 弯成阿基米德螺线：x 沿螺线作为角度、y 作为轨道内的半径，
自带角度轴和径向刻度轴，支持 facet_wrap() / facet_grid()。

- [`coord_spiral()`](https://junjunlab.github.io/ggutils/reference/coord_spiral.md)
  : Spiral coordinates for ggplot2
- [`scale_x_spiral_axis()`](https://junjunlab.github.io/ggutils/reference/scale_spiral_axis.md)
  [`scale_y_spiral_axis()`](https://junjunlab.github.io/ggutils/reference/scale_spiral_axis.md)
  : Fine-tune the axes drawn along a spiral

## 聚类树 (Cluster trees)

给 x / y 分类轴加一棵层次聚类树：配套 scale 用聚类叶序重排分类顺序，
树与数据严格对齐；分面时按面板逐棵重算，树自动贴到 strip 内侧。

- [`guide_axis_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md)
  [`scale_x_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md)
  [`scale_y_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md)
  : Cluster axis guide for ggplot2
- [`cluster_order()`](https://junjunlab.github.io/ggutils/reference/cluster_order.md)
  : Retrieve the dendrogram leaf order
- [`strip_cluster()`](https://junjunlab.github.io/ggutils/reference/strip_cluster.md)
  : Keep cluster trees inside the facet strip

## 分面 (Facets)

逐面板自由的 facet_grid：scales = “free_x” / “free_y” / “free”
按面板放开， 而不是像 facet_grid() 那样按列 / 按行自由。

- [`facet_gridn()`](https://junjunlab.github.io/ggutils/reference/facet_gridn.md)
  [`FacetGridN`](https://junjunlab.github.io/ggutils/reference/facet_gridn.md)
  :

  A `facet_grid()` that can free scales panel by panel

## 排布算法 (Layout algorithms)

guide 内部使用的三个底层排布算法：一维防重叠”压紧”
（circlize::smartAlign 的移植）、盒式排布 （ComplexHeatmap::BoxArrange
的移植）以及把它们串起来的 多行堆叠。它们也可以在包外单独调用。

- [`smart_align()`](https://junjunlab.github.io/ggutils/reference/smart_align.md)
  : 一维标签盒防重叠压紧算法（smartAlign 移植）
- [`box_arrange()`](https://junjunlab.github.io/ggutils/reference/box_arrange.md)
  : 一维标签盒聚类排布算法（BoxArrange 移植）
- [`smart_stack()`](https://junjunlab.github.io/ggutils/reference/smart_stack.md)
  : 多行标签排布引擎

## 内置数据 (Datasets)

示例和 vignette 使用的真实数据：GEO GSE52778 的 RNA-seq 热图 （300
个高变基因 × 8 个样本的行 z-score 矩阵），以及 Seurat PBMC3k 教程的
marker 表与 dot plot 数据。

- [`airway_heatmap`](https://junjunlab.github.io/ggutils/reference/airway_heatmap.md)
  : Processed RNA-seq expression matrix (GEO GSE52778)
- [`pbmc3k_markers`](https://junjunlab.github.io/ggutils/reference/pbmc3k_markers.md)
  : PBMC3k single-cell marker table
- [`pbmc3k_dotplot`](https://junjunlab.github.io/ggutils/reference/pbmc3k_dotplot.md)
  : PBMC3k dot plot data
