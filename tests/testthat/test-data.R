## 内置数据 airway_heatmap 的结构契约（文档 R/data.R 中描述的内容必须成立）

test_that("airway_heatmap 的结构与文档一致", {
  data(airway_heatmap, package = "ggsmartext")

  expect_type(airway_heatmap, "list")
  expect_named(
    airway_heatmap,
    c("z", "genes", "samples", "df", "mark", "mark_by_set")
  )

  ## z：300 x 8 的行 z-score 矩阵，行列名与注释一致
  expect_true(is.matrix(airway_heatmap$z))
  expect_identical(dim(airway_heatmap$z), c(300L, 8L))
  expect_identical(rownames(airway_heatmap$z), airway_heatmap$genes$gene)
  expect_identical(colnames(airway_heatmap$z), airway_heatmap$samples$sample)

  ## 逐基因标准化：行均值≈0、行标准差≈1
  expect_true(all(abs(rowMeans(airway_heatmap$z)) < 1e-8))
  expect_equal(
    unname(apply(airway_heatmap$z, 1L, stats::sd)),
    rep(1, 300L),
    tolerance = 1e-8
  )

  ## 注释表
  expect_identical(
    names(airway_heatmap$genes), c("gene", "gene_set", "delta")
  )
  expect_identical(
    names(airway_heatmap$samples), c("sample", "cell", "dex")
  )
  expect_identical(
    airway_heatmap$samples$sample,
    paste(airway_heatmap$samples$cell, airway_heatmap$samples$dex, sep = "_")
  )
  expect_setequal(airway_heatmap$samples$dex, c("untrt", "trt"))
  expect_identical(length(unique(airway_heatmap$samples$cell)), 4L)
  expect_false(anyDuplicated(airway_heatmap$genes$gene) > 0L)
  expect_setequal(
    unique(airway_heatmap$genes$gene_set),
    c("up in dexamethasone", "down in dexamethasone")
  )

  ## 绘图长表：300 x 8 行，gene 水平为倒序（第一簇画在最上方）
  expect_identical(nrow(airway_heatmap$df), 2400L)
  expect_identical(
    names(airway_heatmap$df), c("gene", "sample", "gene_set", "z")
  )
  expect_identical(
    levels(airway_heatmap$df$gene), rev(airway_heatmap$genes$gene)
  )
  expect_identical(
    levels(airway_heatmap$df$sample), airway_heatmap$samples$sample
  )
  expect_identical(
    levels(airway_heatmap$df$gene_set),
    c("up in dexamethasone", "down in dexamethasone")
  )
  expect_equal(
    sort(unique(as.character(airway_heatmap$df$gene))),
    sort(airway_heatmap$genes$gene)
  )
})

test_that("airway_heatmap 的示例标注都指向真实存在的基因", {
  data(airway_heatmap, package = "ggsmartext")
  genes <- airway_heatmap$genes$gene

  ## 单面板示例：20 个基因
  expect_length(airway_heatmap$mark, 20L)
  expect_true(all(airway_heatmap$mark %in% genes))
  expect_false(anyDuplicated(airway_heatmap$mark) > 0L)

  ## 分面示例：两个基因集各 4 个，且与各自的 gene_set 对应
  expect_length(airway_heatmap$mark_by_set, 2L)
  expect_setequal(
    names(airway_heatmap$mark_by_set),
    c("up in dexamethasone", "down in dexamethasone")
  )
  for (set in names(airway_heatmap$mark_by_set)) {
    ids <- airway_heatmap$mark_by_set[[set]]
    expect_length(ids, 4L)
    expect_true(all(ids %in% genes))
    expect_true(all(
      airway_heatmap$genes$gene_set[match(ids, genes)] == set
    ))
  }
})
