# ggsmartext 0.0.1

* 首个版本。

* 内置数据 `airway_heatmap`：GEO GSE52778 的真实 RNA-seq 表达矩阵（300 个
  变异最大的基因，逐基因 z-score，按层次聚类排序），同时带上绘图用的长表
  `df`、样本/基因注释以及示例标注 `mark` / `mark_by_set`。文档与示例直接
  `data(airway_heatmap)` 加载后画图，渲染时不再需要 `airway` 和
  `SummarizedExperiment`，两者也已从 `Suggests` 移除。

* `guide_axis_smart()`：ggplot2 的 `GuideAxis` 子类，用于分类轴标签过多时按需
  显示标签、多行/多列错开、并用引导线把每个标签连回自己的刻度。

  - `which` 支持整数下标、字符标签、逻辑向量、函数四种写法；`n` 自动等距挑
    最多 n 个标签。
  - 轨道数自动：能一行放下一行（沿轴展开），标签挤在集中区域时自动分层；
    `nrow` 可强制轨道数。
  - 引导线默认 `connection_style = "diagonal"`，几何与
    `ComplexHeatmap::anno_mark()` 一致（短横线 → 斜线 → 短横线 → 文字），
    `link_length` 控制连线长度；另有 `"straight"` 与 `"elbow"`。
  - `extend = 0`（默认）保证标签不越出面板被裁切；旋转标签（`angle`）的轨道
    高度按旋转后的包围盒计算。
  - 支持 x 轴、y 轴、顶部/底部/左侧/右侧轴，以及 `sec.axis`（如热图右侧基因
    标注）。与 `facet`、`coord_flip`、`theme` 协作。

* `coord_radial()`（ggplot2 >= 3.5 / 本包要求的 ggplot2 4.0 中均可用）：

  - theta 轴（`theta = "x"` 或 `"y"`）只接受认识 `theta` aesthetic 的 guide，
    `guide_axis_smart()` 现在就是其中之一：设置 `available_aes = c("x", "y",
    "theta")`，在环形坐标里把标签沿圆周排布、随角度旋转（复用 ggplot2 的
    `GuideAxisTheta` 绘制），并继续支持 `which` / `n` 选择与 `tick = "shown"`；
  - 使用 `guides(theta = guide_axis_smart(...))` 挂载；
  - 环形坐标下 `nrow` 轨道与引导线是直角坐标的特性，会被忽略（标签直接排在圆周上）；
  - 参考手册示例中新增 `coord_radial(theta = "x")` 与 `theta = "y"` 两个可运行示例，
    测试见 `tests/testthat/test-guide-radial.R`。

* 分面（`facet_wrap()` / `facet_grid()`）：

  - 每个面板各自建立 guide：`free_x` / `free_y` 时只画该面板内存在的标签，
    `which` 一次写全所有面板的标签即可，不存在的标签静默跳过（不再逐面板
    警告），详见 `vignettes/ggsmartext.Rmd` 的“与 facet 配合”一节。
  - 面板内拥挤的标签按该面板宽度自动分层，也可用 `nrow` 强制。
  - `facet_grid` 行分面共享 x 轴时 guide 只画一次；列分面共享 y 轴同理。
  - 热图分面时把 guide 挂在 `scale_y_discrete(position = "right", guide = ...)`
    上（`dup_axis()` 在分面里不会逐面板绘制）。
  - 新增分面测试 `tests/testthat/test-guide-facets.R`（柱状图 facet_wrap /
    facet_grid、free 刻度逐面板选标签、面板内拥挤分层、分面热图右侧 y 轴），
    以及示例 `inst/examples/10-facets.R`（4 张图）。

* 内置数据 `airway_heatmap`（GEO GSE52778，来自 Bioconductor `airway`）：

  - 一次性预处理成 300 个高变基因 × 8 个样本的行 z-score 矩阵（已按层次
    聚类排序），附带单图标注的 20 个基因 `mark` 与分面用的每组 4 个基因
    `mark_by_set`；
  - vignette 与 `inst/examples/09-heatmap-rnaseq.R` 直接用它，渲染时不再
    依赖 `airway` / `SummarizedExperiment`（两者已从 Suggests 移除），
    CI 也不再需要 Bioconductor 依赖；
  - 数据生成脚本见 `.scratch/make_data.R`（不进包），结构契约测试见
    `tests/testthat/test-data.R`。

* 底层算法（纯 R，无 Rcpp）：

  - `smart_align()`：`circlize::smartAlign` 的精确移植，一维标签盒防重叠压紧。
  - `box_arrange()`：`ComplexHeatmap` 的 `BoxArrange` 纯函数化移植，聚类扁平化
    排布（允许乱序输入）。
  - `smart_stack()`：多行标签排布引擎（优先级 + 负载均衡分行，行内保序压紧，
    容量不足时丢弃而不是压扁）。

* 文档：中文 vignette（`vignette("ggsmartext")`）、英文 vignette
  （`vignette("ggsmartext-en")`）、英文 README、22 张示例图
  （`inst/examples/07-gallery.R`）。热图标注示例使用真实 GEO RNA-seq 数据
  （`airway`，GEO GSE52778，见 `inst/examples/09-heatmap-rnaseq.R`）。

* 需要 R >= 4.1、ggplot2 >= 4.0.0。
