# 文本测量与真正的绘制都需要一个图形设备。空 PDF 设备不会写任何文件，
# 这样测试既可以在有设备的会话里跑，也不会在测试目录里留下 Rplots.pdf。
if (grDevices::dev.cur() == 1L) {
  grDevices::pdf(file = NULL)
}

# ggplot2 只在这里加载一次，避免每个测试文件重复 attach 时反复出现
# "package was built under R version ..." 的警告。
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
