# 一维标签盒聚类排布算法（BoxArrange 移植）

`box_arrange()` 把 ComplexHeatmap 内部 R5 引用类 `BoxArrange` （以及
`Box` / `BoxCluster` 两个辅助类）**纯函数化**重写：

## Usage

``` r
box_arrange(start, end, range = range(c(start, end)))
```

## Arguments

- start, end:

  数值向量，标签盒的左右端点，长度必须相同； 不要求按 `start` 排序。

- range:

  长度为 2 的数值向量，允许排布的范围（如 `c(0, 1)`）。 默认
  `range(c(start, end))`。

## Value

一个 data.frame，含 `start`、`end` 两列，**行顺序与输入完全 一致**（第 i
行是输入第 i 个盒子的新位置）。

## Details

1.  按每个盒子的**中点**排序（不要求输入有序）；

2.  依中点次序把相互重叠（含首尾相接）的盒子并成 cluster；

3.  每个 cluster 先用 `sum(width)` 作为总长，在 `range` 内首尾相接摊开
    （`flatten`），再迭代合并因贴边平移而新产生的重叠（`merge`）；

4.  把越界的 cluster 整体平移回
    `range`（`adjust_to_range`），再合并一次；

5.  按原始输入顺序还原每个盒子的位置。

若所有盒子宽度之和大于 `diff(range)`（根本放不下），则退回
`ComplexHeatmap:::smartAlign2()` 的**等比摊开**公式：保持首尾盒贴住
两端、按宽度比例拉开，结果允许重叠但全部落在 `range` 内。

与参考实现（`ComplexHeatmap:::smartAlign2()`，即 `BoxArrange` 的包装）
在可行输入上逐元素一致（容差 1e-10）。差异均为健壮性/可重复性改进：

- 空输入返回 0 行 data.frame；参考实现报 `subscript out of bounds`；

- 中点完全相同的盒子用**确定性**次序打破平局（参考实现用
  `rank(ties.method = "random")`，会消耗随机数、结果不可重复）；
  只有在存在精确平局时结果才可能与参考实现不同；

- 单个盒子且宽度大于 `range` 时，居中放置；参考实现返回 `-Inf`。

参考实现的可行分支返回矩阵、不可行分支返回 data.frame，本函数统一 返回
data.frame。

## See also

[`smart_align()`](https://junjunlab.github.io/ggsmartext/reference/smart_align.md)：circlize
的压紧算法（要求输入有序）；
[`smart_stack()`](https://junjunlab.github.io/ggsmartext/reference/smart_stack.md)：多行标签排布引擎。

## Examples

``` r
# 两个轻微重叠的盒子被压成首尾相接
box_arrange(c(0.10, 0.12), c(0.14, 0.16), c(0, 1))
#>   start  end
#> 1  0.09 0.13
#> 2  0.13 0.17

# 允许乱序输入：按中点排序后处理，结果仍按输入顺序返回
box_arrange(c(0.60, 0.10, 0.30), c(0.64, 0.14, 0.34), c(0, 1))
#>   start  end
#> 1   0.6 0.64
#> 2   0.1 0.14
#> 3   0.3 0.34

# 宽度之和超过 range 时退回等比摊开（首尾贴边）
box_arrange(c(0.1, 0.2, 0.3), c(0.6, 0.7, 0.8), c(0, 1))
#>   start  end
#> 1  0.00 0.50
#> 2  0.25 0.75
#> 3  0.50 1.00
```
