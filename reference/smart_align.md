# 一维标签盒防重叠压紧算法（smartAlign 移植）

`smart_align()` 是 circlize 包中 `smartAlign()` 算法的精确移植：
给定一组按位置升序排列、允许相互重叠的一维区间（标签盒），
通过**迭代聚类 + 压紧**的方式消除重叠：相互重叠的盒子被并成一个
cluster，cluster 内按原有宽度首尾相接重新排布，并把 cluster 整体
平移到其原始中点附近；只要不越界就尽量少移动。迭代直到 cluster
的数目不再变化为止。

## Usage

``` r
smart_align(start, end, range = range(c(start, end)))
```

## Arguments

- start, end:

  数值向量，标签盒的左右端点（任意一致的单位， 例如 npc
  或数据坐标）。两个向量长度必须相同。`start` 必须按
  升序排列（这是算法假设，与参考实现一致；乱序输入请先
  [`order()`](https://rdrr.io/r/base/order.html)，或改用
  [`box_arrange()`](https://junjunlab.github.io/ggsmartext/reference/box_arrange.md)——它会自行按中点排序）。

- range:

  长度为 2 的数值向量，允许排布的范围（如 `c(0, 1)`）。 默认
  `range(c(start, end))`。

## Value

一个 data.frame，含 `start`、`end` 两列，**行顺序与输入完全 一致**（第 i
行是输入第 i 个盒子的新位置）。

## Details

该函数是纯函数（不修改环境、不使用随机数），结果可重复。

与参考实现的差异只有两处，均为健壮性改进：

- 空输入返回 0 行的
  data.frame（[`circlize::smartAlign()`](https://rdrr.io/pkg/circlize/man/smartAlign.html)
  会报错）；

- 输入不合法（长度不一致、含非有限值、`range` 非法）时给出清晰的报错。

注意：若所有盒子宽度之和大于 `diff(range)`，算法无法消除重叠，
此时（与参考实现一致）会把该 cluster 居中放置，结果会**超出**
`range`。调用方应在调用前自己判断可行性（[`smart_stack()`](https://junjunlab.github.io/ggsmartext/reference/smart_stack.md)
会先丢弃 低优先级标签再调用本函数）。

## See also

[`box_arrange()`](https://junjunlab.github.io/ggsmartext/reference/box_arrange.md)：另一种（`ComplexHeatmap`
的 BoxArrange）
排布算法，允许输入乱序；[`smart_stack()`](https://junjunlab.github.io/ggsmartext/reference/smart_stack.md)：多行标签排布引擎。

## Examples

``` r
# 两个盒子轻微重叠：被压成首尾相接的一对
smart_align(c(0.10, 0.12), c(0.14, 0.16), c(0, 1))
#>   start  end
#> 1  0.09 0.13
#> 2  0.13 0.17

# 四个完全重叠的盒子：在原始中点处摊开
smart_align(rep(0.5, 4), rep(0.6, 4), c(0, 1))
#>   start  end
#> 1  0.35 0.45
#> 2  0.45 0.55
#> 3  0.55 0.65
#> 4  0.65 0.75

# 不重叠的输入原样返回
smart_align(c(0.1, 0.5), c(0.2, 0.6), c(0, 1))
#>   start end
#> 1   0.1 0.2
#> 2   0.5 0.6
```
