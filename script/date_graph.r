library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)

log_path <- "data/hoge-1.log" # ここにログファイル名

lines <- readLines(log_path, warn = FALSE)
objs <- lapply(lines, function(x) {
  x <- trimws(x)
  if (nchar(x) == 0) {
    return(NULL)
  }
  tryCatch(fromJSON(x), error = function(e) NULL)
})
objs <- Filter(Negate(is.null), objs)

df <- bind_rows(objs) %>%
  filter(!is.na(time), !is.na(used_heap_size), !is.na(external_memory)) %>%
  mutate(
    time = as.POSIXct(time, format = "%Y/%m/%d %H:%M:%OS", tz = "Asia/Tokyo"),
    used_heap_size = as.numeric(used_heap_size),
    external_memory = as.numeric(external_memory)
  ) %>%
  arrange(time)

plot_df <- df %>%
  select(time, used_heap_size, external_memory) %>%
  pivot_longer(
    cols = c(used_heap_size, external_memory),
    names_to = "metric", values_to = "value"
  )

ggplot(plot_df, aes(x = time, y = value, color = metric)) +
  geom_line() +
  labs(
    title = "used_heap_size & external_memory over time",
    x = "time", y = "size (MB?)"
  ) +
  theme_minimal()
