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

示例和 vignette 使用的真实 RNA-seq 热图数据（GEO GSE52778）， 已预处理成
300 个高变基因 × 8 个样本的行 z-score 矩阵。

- [`airway_heatmap`](https://junjunlab.github.io/ggutils/reference/airway_heatmap.md)
  : Processed RNA-seq expression matrix (GEO GSE52778)
