# ggsmartext 0.0.1

* 首个版本。

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
