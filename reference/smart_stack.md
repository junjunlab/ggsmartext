# 多行标签排布引擎

`smart_stack()` 把一系列标签盒（每个盒子有轴线上的中心位置 `positions`
与宽度 `widths`，单位均为 npc）分配到 `n` 行（y 轴时为 `n` 列）中的某
一行，使每个标签都能画出来、行内不重叠、并且尽量贴近原始位置。它是
[`guide_axis_smart()`](https://junjunlab.github.io/ggsmartext/reference/guide_axis_smart.md)
的核心排布算法，也可以单独使用。

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

  数值向量，每个标签盒在轴上的中心位置（npc，0..1）。 不要求有序。

- widths:

  数值向量，每个标签盒的宽度（npc），与 `positions` 等长。

- n:

  正整数，行数（x 轴为行、y 轴为列）。也决定"总容量"上限。

- range:

  长度为 2 的数值向量，轴的可绘制范围，默认 `c(0, 1)`。

- extend:

  非负标量：允许标签超出 `range` 的比例（`0.02` 表示两侧各 放宽轴跨度的
  0.02 倍），与
  [`guide_axis_smart()`](https://junjunlab.github.io/ggsmartext/reference/guide_axis_smart.md)
  的同名参数一致。

- padding:

  非负标量：同一行内相邻标签盒之间要求的最小间隙， 单位与 `positions`
  相同（npc）。

- priority:

  `NULL`（默认，所有标签同等重要）或与 `positions` 等长
  的数值向量。**数值越大越优先保留**；放不下时优先丢弃低优先级标签。

## Value

一个 data.frame，行数与输入等长、顺序与输入一致，列为：

- `index`：输入下标（`1..length(positions)`）；

- `row`：分配到的行号（`1..n`）；被丢弃的标签为 `NA`；

- `start`, `end`：排布后标签盒的左右端点（npc）；

- `center`：排布后标签盒中心（npc）；

- `shown`：逻辑值，该标签是否应当绘制。

## Details

算法分三步：

1.  **可用范围**：可用范围 `range` 按 `extend` 向两侧各扩展
    `extend * diff(range)`，得到 `lim`；宽度大于 `diff(lim)` 的标签盒
    无论如何放不下，直接丢弃。

2.  **分配行**：按优先级从高到低（`priority` 越大越优先；平局按位置、
    再按下标）逐个分配。对每个标签盒，在所有"还有容量"的行里选择当前
    占用最少的一行（占用 = 行内宽度之和 + 必要的 `padding` 间隙），因此
    当所有标签优先级相同时，结果会沿轴交替落入各行（水填式均衡）。
    所有行都放不下的标签被丢弃（`shown = FALSE`）。

3.  **行内压紧**：每行内按标签盒中心位置保持轴序，把盒子左右各按
    `padding / 2` 膨胀后交给压紧算法消除重叠，再收缩回来，于是同一行内
    相邻标签之间恰好留出 `padding` 的间隙。膨胀前会用保序投影把中心夹进
    可用范围，因此结果绝不会越出 `lim`。

- 所有 `shown = TRUE` 的盒子都满足：`lim[1] <= start < end <= lim[2]`；
  同一行内按 `start` 排序后满足 `start[i+1] >= end[i] + padding`（在没有
  触发贴边剪裁时取等号）。

- 行内顺序按标签盒**中心位置**保持轴序，与输入位置顺序一致。

- 行的分配由优先级 + 负载均衡决定，是**确定性**的；同一输入必然得到
  同一输出，不消耗随机数。

- 函数保证"能画就不重叠"，但不会为了塞下更多标签而把标签压得比 `widths`
  更窄——放不下就丢弃（`shown = FALSE`）。

- 当标签很多、`n` 行总容量不足时，被丢弃的是优先级最低（`priority`
  最小）的那些；优先级相同时，先处理靠轴左侧的标签。

## See also

[`smart_align()`](https://junjunlab.github.io/ggsmartext/reference/smart_align.md),
[`box_arrange()`](https://junjunlab.github.io/ggsmartext/reference/box_arrange.md)

## Examples

``` r
# 6 个标签均匀分布，分 2 行：交替落入两行，几乎不需要移动
pos <- seq(0.1, 0.9, length.out = 6)
smart_stack(pos, widths = rep(0.08, 6), n = 2)
#>   index row start  end center shown
#> 1     1   1  0.06 0.14   0.10  TRUE
#> 2     2   2  0.22 0.30   0.26  TRUE
#> 3     3   1  0.38 0.46   0.42  TRUE
#> 4     4   2  0.54 0.62   0.58  TRUE
#> 5     5   1  0.70 0.78   0.74  TRUE
#> 6     6   2  0.86 0.94   0.90  TRUE

# 3 个标签挤在 0.5 附近，分 3 行也不会重叠
smart_stack(c(0.48, 0.5, 0.52), widths = rep(0.2, 3), n = 3)
#>   index row start  end center shown
#> 1     1   1  0.38 0.58   0.48  TRUE
#> 2     2   2  0.40 0.60   0.50  TRUE
#> 3     3   3  0.42 0.62   0.52  TRUE

# 容量不足时按 priority 丢弃：高优先级保留
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
