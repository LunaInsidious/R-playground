library(showtext)

# 日本語フォントが使えるようにフォントと描画設定を準備
jp_family <- "Noto Sans CJK JP"

showtext_auto(enable = TRUE)
par(family = jp_family)

csv_path <- "data/hoge.csv"
col_name <- "length"
img_dir <- "img/"

# CSV から対象列を読み込み、欠損や負値を取り除く
df <- read.csv(csv_path)
stopifnot(col_name %in% names(df))
sizes <- df[[col_name]]
sizes <- sizes[is.finite(sizes) & !is.na(sizes) & sizes >= 0]
if (length(sizes) == 0) stop("length 列に有効なデータがありません。")

# 2 の冪で区切ったビン境界とラベルを計算
# 欠損済みのデータに対し、少なくとも 2^0 を含む最大指数を求める
max_exp <- ceiling(log2(max(pmax(sizes, 1))))
exps <- 0:max_exp
breaks <- c(0, 2^(exps + 1))

# ビンごとの件数と合計サイズを算出
bins <- cut(sizes, breaks = breaks, right = FALSE, include.lowest = TRUE)
counts <- as.numeric(table(bins))
sums <- as.numeric(tapply(sizes, bins, sum))
sums[is.na(sums)] <- 0

# ビンごとの件数・合計と「2^n 以上」の累積割合を算出
x_labels <- paste0("2^", exps)
# 「しきい値以上」の件数・サイズを逆順で累積し、割合に変換
tail_counts <- rev(cumsum(rev(counts)))
tail_pct_cnt <- 100 * tail_counts / sum(counts)
tail_sums <- rev(cumsum(rev(sums)))
tail_pct_sum <- 100 * tail_sums / sum(sums)

today <- format(Sys.Date(), "%Y%m%d")

size_units <- c("B", "KB", "MB", "GB", "TB", "PB", "EB")
max_sum <- max(sums)
# 最大値に応じて表示単位を KB/MB ... に切り替える
if (!is.finite(max_sum) || max_sum <= 0) {
  size_unit_idx <- 0
} else {
  size_unit_idx <- floor(log(max_sum, 1024))
  size_unit_idx <- max(0, min(size_unit_idx, length(size_units) - 1))
}
size_unit <- size_units[size_unit_idx + 1]
size_divisor <- 1024^size_unit_idx

format_size <- function(bytes) {
  scaled <- bytes / size_divisor
  if (!is.finite(scaled) || scaled <= 0) {
    return("0")
  }
  digits <- ifelse(scaled >= 100, 1, ifelse(scaled >= 10, 2, 3))
  formatC(scaled, format = "f", digits = digits, big.mark = ",", decimal.mark = ".")
}

# 出力ファイル名を作成
outfile <- paste0(img_dir, "dist_2exp_bars_", today, ".png")
png(outfile, width = 1600, height = 800, res = 150)

# 件数分布と合計サイズ分布を棒グラフと累積割合の折れ線で可視化
# 件数・サイズの 2 つのパネルを並べて描画
par(mfrow = c(1, 2), mar = c(8, 4, 4, 4) + 0.5)

bar_pos1 <- barplot(counts,
  names.arg = x_labels, las = 2,
  ylab = "件数",
  main = "件数分布（2^nビン）と ≥2^n の割合",
  cex.names = 0.8,
  axes = FALSE
)
count_ticks <- pretty(c(0, counts))
axis(2,
  at = count_ticks,
  labels = format(count_ticks, big.mark = ",", trim = TRUE, scientific = FALSE),
  las = 1
)
# X 軸にはビンごとの「以上」割合と件数を文字列で付与
axis(
  side = 1, at = bar_pos1,
  labels = paste0(
    sprintf("%.1f%%", tail_pct_cnt),
    " (",
    format(tail_counts, big.mark = ","),
    "件)"
  ),
  line = 4, las = 2, tick = FALSE, cex.axis = 0.7
)
par(new = TRUE)
plot(bar_pos1, tail_pct_cnt,
  type = "b", pch = 16,
  axes = FALSE, xlab = "", ylab = "", ylim = c(0, 100)
)
axis(4)
mtext("≥2^n の割合（%）", side = 4, line = 3)

# サイズ値を選択済み単位にスケーリング
sums_scaled <- sums / size_divisor

bar_pos2 <- barplot(sums_scaled,
  names.arg = x_labels, las = 2,
  ylab = paste0("合計サイズ（", size_unit, "）"),
  main = "合計サイズ分布（2^nビン）と ≥2^n の割合",
  cex.names = 0.8,
  axes = FALSE
)
sum_ticks <- pretty(c(0, sums_scaled))
axis(2,
  at = sum_ticks,
  labels = format(sum_ticks, big.mark = ",", trim = TRUE, scientific = FALSE),
  las = 1
)
# X 軸にはビンごとの「以上」割合と合計サイズを表示
axis(
  side = 1, at = bar_pos2,
  labels = paste0(
    sprintf("%.1f%%", tail_pct_sum),
    " (",
    vapply(tail_sums, function(x) paste0(format_size(x), " ", size_unit), character(1)),
    ")"
  ),
  line = 4, las = 2, tick = FALSE, cex.axis = 0.7
)
par(new = TRUE)
plot(bar_pos2, tail_pct_sum,
  type = "b", pch = 16,
  axes = FALSE, xlab = "", ylab = "", ylim = c(0, 100)
)
axis(4)
mtext("≥2^n の割合（%）", side = 4, line = 3)

dev.off()
cat("書き出しました: ", normalizePath(outfile), "\n")
