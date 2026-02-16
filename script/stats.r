suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

# --- IEC (base-1024) formatter: B / KiB / MiB / GiB ...
format_iec <- function(x) {
  x <- as.numeric(x)
  units <- c("B", "KiB", "MiB", "GiB", "TiB", "PiB")

  # handle NA/Inf safely
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & is.finite(x) & x >= 0
  if (!any(ok)) {
    return(out)
  }

  xv <- x[ok]
  power <- ifelse(xv > 0, floor(log(xv, 1024)), 0)
  power <- pmin(power, length(units) - 1)

  scaled <- xv / (1024^power)

  # choose decimals by magnitude to keep labels compact
  fmt <- ifelse(scaled >= 100, "%.0f %s",
    ifelse(scaled >= 10, "%.1f %s", "%.2f %s")
  )

  out[ok] <- sprintf(fmt, scaled, units[power + 1])
  out
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("使い方: Rscript stats.r <ndjson_file> [bins] [top_k]")

file_path <- args[[1]]
bins <- if (length(args) >= 2) as.integer(args[[2]]) else 30L
if (is.na(bins) || bins <= 0) bins <- 30L

top_k <- if (length(args) >= 3) as.integer(args[[3]]) else 25L
if (is.na(top_k) || top_k <= 0) top_k <- 25L

# pageを指定している場合は、k+1～k*2件を表示する
page <- if (length(args) >= 4) as.integer(args[[4]]) else 1L
if (is.na(page) || page <= 0) page <- 1L
start_idx <- (page - 1) * top_k + 1
end_idx <- page * top_k

outfile <- paste0("img/category_length_hist_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")

# --- load NDJSON ---
con <- file(file_path, "r")
df_raw <- jsonlite::stream_in(con, verbose = TRUE)
close(con)

if (!all(c("category", "length") %in% names(df_raw))) {
  stop("入力に category または length 列が存在しません")
}

# summary 行があれば除外
if ("_summary" %in% names(df_raw)) {
  df_raw <- df_raw[is.na(df_raw$`_summary`) | df_raw$`_summary` != TRUE, ]
}

# --- clean ---
df <- df_raw %>%
  transmute(
    category = as.character(category),
    length   = suppressWarnings(as.numeric(length))
  ) %>%
  filter(
    !is.na(category), nzchar(category),
    !is.na(length), is.finite(length), length >= 0
  )

if (nrow(df) == 0) stop("有効なレコードがありません。")

# --- summarize all categories ---
summary_all <- df %>%
  group_by(category) %>%
  summarise(
    n = n(),
    mean = mean(length),
    .groups = "drop"
  )

# --- pick top_k by mean (DESC) + add mean label in IEC units ---
summary_df <- summary_all %>%
  arrange(desc(mean)) %>%
  slice(start_idx:end_idx) %>%
  mutate(
    mean_label = format_iec(mean),
    category_label = paste0(category, "\nmean=", mean_label)
  )

cat("=== category ごとの length 集計（平均上位 ", start_idx, "～", end_idx, "）===\n", sep = "")
summary_df %>%
  select(category, mean_label) %>%
  mutate(rank = row_number() + start_idx - 1) %>%
  select(rank, category, mean_label) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

# --- join labels back, enforce ordering (mean DESC) ---
label_levels <- summary_df$category_label

df_plot <- df %>%
  semi_join(summary_df, by = "category") %>%
  left_join(summary_df %>% select(category, category_label), by = "category") %>%
  mutate(
    category_label = factor(category_label, levels = label_levels),
    length_adj = length + 1 # log(0)回避
  )

# --- output dir ---
dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)

# --- plot sizing ---
n_cat <- nlevels(df_plot$category_label)
ncol <- 5
nrow <- ceiling(n_cat / ncol)
plot_width <- 22
plot_height <- max(12, nrow * 3.8)

x_max <- max(df_plot$length_adj, na.rm = TRUE)

# 1,10,100 を 1024^k に掛ける（= 1KiB,10KiB,100KiB, ... の系列）
max_pow_1024 <- ceiling(log(x_max, 1024))
breaks <- sort(unique(as.numeric(outer(c(1, 10, 100), 1024^(0:max_pow_1024), `*`))))
breaks <- breaks[breaks >= 1 & breaks <= x_max]

# --- plot ---
p <- ggplot(df_plot, aes(x = length_adj)) +
  geom_histogram(bins = bins, fill = "#3B82F6", color = "white", linewidth = 0.2) +
  facet_wrap(~category_label, scales = "free_y", ncol = ncol) +
  scale_x_log10(
    breaks = breaks,
    labels = format_iec
  ) +
  labs(
    title = paste0("category ごとの length 分布（平均上位 ", start_idx, "～", end_idx, "）"),
    subtitle = paste0(
      "records = ", nrow(df_plot), "/", nrow(df),
      ", categories = ", n_cat,
      ", bins = ", bins
    ),
    x = "length",
    y = "count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 9, angle = 0, vjust = 1),
    axis.title.x = element_text(margin = margin(t = 8)),
    plot.margin = margin(10, 10, 18, 10)
  )

ggsave(outfile, p, width = plot_width, height = plot_height, dpi = 150, bg = "white")
cat("\nヒストグラムを書き出しました: ", normalizePath(outfile), "\n", sep = "")
