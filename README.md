
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggsmartext

> Smart axis labels for ggplot2: show only the categories you care
> about, connect each label to its tick with a leader line, and stagger
> the labels over several rows so that the space outside the axis is
> actually used.

When a discrete axis carries many categories (50 samples, 200 features,
…) the stock `ggplot2` axis draws every label on a single line. The
labels overlap, or you rotate them all by 90°, or you hide them and lose
the information. `ggsmartext` adds `guide_axis_smart()`, a `GuideAxis`
subclass that takes over label drawing:

- **Choose the labels**: `which =` (integer index / label text / logical
  vector / function) or `n =` (auto-pick at most `n` labels, evenly
  spaced).
- **Use the page**: labels are packed without overlap
  (`smartAlign`-style one-dimensional compaction) and distributed over
  `nrow` rows (x axis) or columns (y axis). Rows are only added when
  they are needed: labels stretch along the axis when a single row fits,
  clustered labels are stacked automatically.
- **Never overlaps**: if there is no room left, labels are dropped
  instead of being printed on top of each other.
- **Leader lines**: every displayed label is tied back to its tick, by
  default with the `circlize` / `ComplexHeatmap::anno_mark()` “diagonal”
  geometry (short perpendicular stub, diagonal, short stub);
  `connection_style = "straight"` and `"elbow"` are also available.

## Installation

Requires R \>= 4.1 and **ggplot2 \>= 4.0.0** (the guide API used here
was rewritten in ggplot2 4.0).

``` r
# install.packages("remotes")
remotes::install_github("junjunlab/ggsmartext")
```

Or, from a local clone of the repository:

``` r
# install.packages("remotes")
remotes::install_local(".")
# or, from a shell:
# R CMD INSTALL .
```

## Quick start

``` r
library(ggplot2)
library(ggsmartext)

set.seed(2024)
df <- data.frame(
  sample = factor(paste0("样本", 1:50), levels = paste0("样本", 1:50)),
  value = round(runif(50, 10, 100), 1)
)

ggplot(df, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  guides(x = guide_axis_smart(n = 10, nrow = 2)) +
  labs(x = NULL, y = "response",
       title = "guide_axis_smart(n = 10, nrow = 2)") +
  theme_minimal(base_size = 12)
```

<div class="figure">

<img src="man/figures/README-quickstart-1.png" alt="guide_axis_smart(n = 10, nrow = 2): 10 labels, two staggered rows, leader lines back to the ticks" width="100%" />
<p class="caption">

guide_axis_smart(n = 10, nrow = 2): 10 labels, two staggered rows,
leader lines back to the ticks
</p>

</div>

No `theme(axis.text.x = element_blank())` needed: the guide draws the
labels itself. The function can also be attached to a scale:
`scale_x_discrete(guide = guide_axis_smart(...))`.

## Pick the labels you need

``` r
ggplot(df, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  guides(x = guide_axis_smart(which = seq(1, 50, by = 10), nrow = 2)) +
  labs(x = NULL, y = "response",
       title = "which = seq(1, 50, by = 10)") +
  theme_minimal(base_size = 12)
```

<div class="figure">

<img src="man/figures/README-which-1.png" alt="which = seq(1, 50, by = 10): every tenth category" width="100%" />
<p class="caption">

which = seq(1, 50, by = 10): every tenth category
</p>

</div>

`which` accepts an integer index vector, a character vector of labels, a
logical vector (one entry per label) or a function
`function(labels, positions)` that returns a logical vector (`positions`
are 0..1 axis-relative coordinates). `n` caps the final number of
labels: `which` selects first, `n` then keeps at most `n` of them evenly
spaced; with `which = NULL`, `n` picks directly from all labels.

## Stagger over several rows

``` r
ggplot(df, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  guides(x = guide_axis_smart(n = 24, nrow = 3)) +
  labs(x = NULL, y = "response", title = "n = 24, nrow = 3") +
  theme_minimal(base_size = 12)
```

<div class="figure">

<img src="man/figures/README-rows-1.png" alt="nrow = 3: three staggered rows make room for more labels" width="100%" />
<p class="caption">

nrow = 3: three staggered rows make room for more labels
</p>

</div>

On a y axis `nrow` means *columns* of labels:

``` r
labs <- paste0("样本", 1:40)
df_y <- data.frame(
  sample = factor(labs, levels = rev(labs)),
  value = round(runif(40, 10, 100), 1)
)

ggplot(df_y, aes(x = value, y = sample)) +
  geom_col(fill = "#F6903D", width = 0.8) +
  guides(y = guide_axis_smart(n = 10, nrow = 2)) +
  labs(x = "response", y = NULL,
       title = "y axis: n = 10, nrow = 2 (2 columns)") +
  theme_minimal(base_size = 12)
```

<div class="figure">

<img src="man/figures/README-yaxis-1.png" alt="y axis: n = 10 labels laid out in nrow = 2 columns" width="100%" />
<p class="caption">

y axis: n = 10 labels laid out in nrow = 2 columns
</p>

</div>

## Long labels: rotate instead of stacking

``` r
trt <- rep(c("对照组", "处理组A", "处理组B"), 8)
labs <- paste0("样本", 1:24, "（", trt, "）")
df_long <- data.frame(
  sample = factor(labs, levels = labs),
  value = round(runif(24, 10, 100), 1)
)

ggplot(df_long, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  guides(x = guide_axis_smart(n = 12, nrow = 1, angle = 45, hjust = 1, vjust = 1)) +
  labs(x = NULL, y = "response", title = "angle = 45, hjust = 1, vjust = 1") +
  theme_minimal(base_size = 12)
```

<div class="figure">

<img src="man/figures/README-angle-1.png" alt="angle = 45, hjust = 1, vjust = 1" width="100%" />
<p class="caption">

angle = 45, hjust = 1, vjust = 1
</p>

</div>

## Heatmap-style annotation (`anno_mark`)

This is the geometry of `ComplexHeatmap::anno_mark()`: hide the full
axis and label only the rows you care about. Duplicate the axis with
`dup_axis()`, switch the main guide off with `guide_none()`, and put
`guide_axis_smart()` on the copy:

``` r
## real RNA-seq data from GEO: airway (GSE52778), 4 cell lines +/- dexamethasone
library(airway)
data(airway, package = "airway")

counts <- SummarizedExperiment::assay(airway, "counts")
cpm <- t(t(counts) / colSums(counts)) * 1e6
logcpm <- log2(cpm[rowSums(cpm > 1) >= 4, ] + 1)

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

ggplot(df_gene, aes(x = sample, y = gene, fill = z)) +
  geom_tile() +
  scale_y_discrete(
    guide = guide_none(),
    sec.axis = dup_axis(
      guide = guide_axis_smart(which = gene_labels[1:8])
    )
  ) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, name = "z-score") +
  labs(x = NULL, y = NULL, title = "airway RNA-seq (GEO GSE52778)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<div class="figure">

<img src="man/figures/README-anno-mark-1.png" alt="Real RNA-seq data (GEO GSE52778): only the 8 glucocorticoid-responsive genes are labelled on a duplicated right axis" width="100%" />
<p class="caption">

Real RNA-seq data (GEO GSE52778): only the 8 glucocorticoid-responsive
genes are labelled on a duplicated right axis
</p>

</div>

Real data from GEO: the
[`airway`](https://bioconductor.org/packages/airway) package holds an
RNA-seq experiment on human airway smooth muscle cells (GEO
[GSE52778](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE52778)),
four cell lines with and without dexamethasone. The eight genes labelled
here are the glucocorticoid-responsive genes that cluster together at
the top of the heatmap. `airway` is a `Suggests` dependency; install it
with `BiocManager::install("airway")` when you re-render this README.

The same works for a cluster of neighbouring categories: select it with
`which = 24:35` and the guide stacks the labels over several rows
automatically (`nrow = 1` forces the single-row `anno_mark` look
instead).

## Main arguments

| argument | default | meaning |
|----|----|----|
| `which` | `NULL` | labels to show: integer index / character / logical / function |
| `n` | `NULL` | hard cap on the number of labels shown (evenly spaced) |
| `nrow` | `NULL` | number of label rows (x axis) or columns (y axis); `NULL` = auto |
| `padding`, `gap` | `0.25`, `0.30` | minimum box spacing / track spacing, relative to label height |
| `extend` | `0` | allow labels to exceed the panel by this fraction (0..0.5); `0` keeps every label inside the panel |
| `connection`, `connection_style` | `TRUE`, `"diagonal"` | draw leader lines; `"diagonal"`, `"straight"` or `"elbow"` |
| `link_length` | `3` | length (mm) of the leader segment between the axis and the first label track |
| `connection_col/lwd/lty` | `NULL`, `0.5`, `1` | leader line style |
| `tick` | `"all"` | ticks: `"all"`, `"shown"` or `"none"` |
| `angle`, `hjust`, `vjust` | `NULL` | label rotation, `NULL` inherits the theme |
| `panel_width` | `NULL` | panel width (cm) used to estimate the number of rows |
| `max_nrow` | `8` | upper bound on the automatic number of label tracks |

## Learn more

- Two vignettes cover the same material: an English walkthrough in
  `vignette("ggsmartext-en", package = "ggsmartext")` and a Chinese one
  in `vignette("ggsmartext", package = "ggsmartext")`.
- The layout algorithms are exported and can be used on their own:
  `smart_align()` (a port of `circlize::smartAlign`), `box_arrange()` (a
  port of `ComplexHeatmap`’s box arrangement) and `smart_stack()`.
- Showcase scripts live in `inst/examples/`; `07-gallery.R` regenerates
  all figures used in the docs.

## Credits

The one-dimensional, overlap-free compaction is a port of the
`smartAlign` algorithm from the **circlize** package; the box
arrangement is a port of the **ComplexHeatmap** package. `ggsmartext`
only re-uses the algorithms — the ggplot2 guide integration is new.
