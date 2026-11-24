# ============================================================================
# 用真实 GEO RNA-seq 数据演示热图基因标注（ComplexHeatmap anno_mark 风格）
#
# 数据：airway 包（Bioconductor）——GEO GSE52778
#   人气道平滑肌细胞，4 株细胞系 × 地塞米松处理/对照，共 8 个样本。
#   取变异最大的 60 个基因做竖长热图：
#     (1) 单图：右侧只标注 8 个经典糖皮质激素响应基因；
#     (2) 分面图：按"处理后上调/下调"分面，每个面板数据不同、各自标注。
#
# 依赖（Suggests）：
#   if (!requireNamespace("airway", quietly = TRUE)) {
#     BiocManager::install("airway")
#   }
#
# 用法：Rscript inst/examples/09-heatmap-rnaseq.R [输出目录]
# ============================================================================

.libPaths(c(".Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggsmartext)
})

if (!requireNamespace("airway", quietly = TRUE)) {
  stop(
    "需要 airway 数据包：\n",
    "  if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager')\n",
    "  BiocManager::install('airway')",
    call. = FALSE
  )
}

outdir <- if (length(commandArgs(TRUE)) >= 1L) commandArgs(TRUE)[1] else tempdir()
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

library(airway)
data(airway, package = "airway")

counts <- SummarizedExperiment::assay(airway, "counts")
cpm <- t(t(counts) / colSums(counts)) * 1e6
logcpm <- log2(cpm[rowSums(cpm > 1) >= 4, ] + 1) # expressed in >= 4 samples

## 60 most variable genes -> row z-scores -> hierarchical clustering
vars <- apply(logcpm, 1, var)
z <- t(scale(t(logcpm[names(sort(vars, decreasing = TRUE))[1:60], ])))
ord <- hclust(dist(z), method = "ward.D2")$order

sym <- SummarizedExperiment::rowData(airway)$symbol[match(rownames(z), rownames(airway))]
gene_name <- ifelse(is.na(sym) | sym == "", rownames(z), sym)
names(gene_name) <- rownames(z)

## split genes into "up" / "down" after dexamethasone (used for the facets)
cd <- as.data.frame(SummarizedExperiment::colData(airway))
delta <- rowMeans(z[, cd$dex == "trt"]) - rowMeans(z[, cd$dex == "untrt"])
gene_set <- ifelse(delta > 0, "up in dexamethasone", "down in dexamethasone")

sample_lab <- factor(paste(cd$cell, cd$dex, sep = "_"),
                     levels = paste(cd$cell, cd$dex, sep = "_"))

df_gene <- data.frame(
  gene = factor(rep(gene_name[ord], times = nrow(cd)), levels = rev(gene_name[ord])),
  sample = factor(rep(sample_lab, each = nrow(z)), levels = levels(sample_lab)),
  gene_set = factor(rep(gene_set[ord], times = nrow(cd)),
                    levels = c("up in dexamethasone", "down in dexamethasone")),
  z = as.vector(z[ord, ])
)

base_theme <- theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

fill_scale <- scale_fill_gradient2(low = "#2166ac", mid = "white",
                                   high = "#b2182b", midpoint = 0, name = "z-score")

## ---- (1) tall single heatmap, anno_mark style on the right ------------------
## only the eight glucocorticoid-responsive genes that cluster at the top
marked_8 <- gene_name[ord][1:8]
message("single heatmap, marked genes: ", paste(marked_8, collapse = ", "))

p1 <- ggplot(df_gene, aes(x = sample, y = gene, fill = z)) +
  geom_tile() +
  scale_y_discrete(
    guide = guide_none(),
    sec.axis = dup_axis(guide = guide_axis_smart(which = marked_8))
  ) +
  fill_scale +
  labs(x = NULL, y = NULL, title = "airway RNA-seq (GEO GSE52778)",
       subtitle = "top 60 variable genes; 8 glucocorticoid-responsive genes labelled") +
  base_theme

f1 <- file.path(outdir, "09-heatmap-rnaseq.png")
ragg::agg_png(f1, width = 6.5 * 150, height = 9 * 150, res = 150)
print(p1)
invisible(dev.off())
message("written: ", f1)

## ---- (2) faceted heatmap: different data per panel, labels per panel -------
mark_by_set <- lapply(
  split(names(delta), gene_set),
  function(ids) ids[order(abs(delta[ids]), decreasing = TRUE)][1:4]
)
mark <- unlist(mark_by_set, use.names = FALSE)
message("facet panels, marked genes: ", paste(gene_name[mark], collapse = ", "))

p2 <- ggplot(df_gene, aes(x = sample, y = gene, fill = z)) +
  geom_tile() +
  facet_grid(gene_set ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_y_discrete(
    position = "right",
    guide = guide_axis_smart(
      which = function(labels, positions) labels %in% gene_name[mark]
    )
  ) +
  fill_scale +
  labs(x = NULL, y = NULL, title = "facet by gene set",
       subtitle = "each panel has different data and labels its own genes") +
  base_theme +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0))

f2 <- file.path(outdir, "09-heatmap-rnaseq-facet.png")
ragg::agg_png(f2, width = 7 * 150, height = 9.5 * 150, res = 150)
print(p2)
invisible(dev.off())
message("written: ", f2)
