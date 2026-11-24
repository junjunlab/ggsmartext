# ============================================================================
# 用真实 GEO RNA-seq 数据演示热图基因标注（ComplexHeatmap anno_mark 风格）
#
# 数据：airway 包（Bioconductor）——GEO GSE52778
#   人气道平滑肌细胞，4 株细胞系 × 地塞米松处理/对照，共 8 个样本。
#   这里取变异最大的 60 个基因做热图，只标注聚类后聚在一起的 8 个
#   糖皮质激素响应基因（TSC22D3 / KLF15 / FKBP5 / PER1 / ...）。
#
# 依赖（不在包 Imports 里，属于 Suggests）：
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
keep <- rowSums(cpm > 1) >= 4 # expressed in at least 4 samples
logcpm <- log2(cpm[keep, ] + 1)

## 60 most variable genes -> row z-scores -> hierarchical clustering
vars <- apply(logcpm, 1, var)
z <- t(scale(t(logcpm[names(sort(vars, decreasing = TRUE))[1:60], ])))
ord <- hclust(dist(z), method = "ward.D2")$order

sym <- SummarizedExperiment::rowData(airway)$symbol[match(rownames(z), rownames(airway))]
gene_labels <- ifelse(is.na(sym[ord]) | sym[ord] == "", rownames(z)[ord], sym[ord])

cd <- as.data.frame(SummarizedExperiment::colData(airway))
sample_lab <- factor(paste(cd$cell, cd$dex, sep = "_"),
                     levels = paste(cd$cell, cd$dex, sep = "_"))

df_gene <- data.frame(
  gene = factor(rep(rev(gene_labels), times = nrow(cd)), levels = rev(gene_labels)),
  sample = factor(rep(sample_lab, each = nrow(z)), levels = levels(sample_lab)),
  z = as.vector(z[ord, ])
)

## only the eight glucocorticoid-responsive genes that cluster at the top
marked <- gene_labels[1:8]
message("marked genes: ", paste(marked, collapse = ", "))

p <- ggplot(df_gene, aes(x = sample, y = gene, fill = z)) +
  geom_tile() +
  scale_y_discrete(
    guide = guide_none(),
    sec.axis = dup_axis(guide = guide_axis_smart(which = marked))
  ) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, name = "z-score") +
  labs(x = NULL, y = NULL, title = "airway RNA-seq (GEO GSE52778)",
       subtitle = "top 60 variable genes; 8 glucocorticoid-responsive genes labelled") +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

file <- file.path(outdir, "09-heatmap-rnaseq.png")
ragg::agg_png(file, width = 1600, height = 900, res = 150)
print(p)
invisible(dev.off())
message("written: ", file)
