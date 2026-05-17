# =========================================================================
# 学术期刊风格图：TCI、NET、TO/FROM
# 仿 JBF / JFE 黑白印刷友好风格
# =========================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# 设置 Times New Roman 字体
windowsFonts(Times = windowsFont("Times New Roman"))

# 读取数据
tci_df <- read.csv("dynamic_TCI.csv")
tci_df$日期 <- as.Date(tci_df$日期)

net_df <- read.csv("dynamic_NET.csv")
net_df$日期 <- as.Date(net_df$日期)

to_df <- read.csv("dynamic_TO.csv")
to_df$日期 <- as.Date(to_df$日期)

from_df <- read.csv("dynamic_FROM.csv")
from_df$日期 <- as.Date(from_df$日期)

hmm_states <- read.csv("HMM_states_composite.csv")
hmm_states$日期 <- as.Date(hmm_states$日期)

# 准备状态背景（使用斜线填充区分，而非颜色）
rle_states <- rle(as.character(hmm_states$state_composite))
state_segments <- data.frame(
  start_idx = c(1, 1 + cumsum(rle_states$lengths)[-length(rle_states$lengths)]),
  end_idx   = cumsum(rle_states$lengths),
  state     = rle_states$values
)
state_segments <- state_segments %>%
  mutate(
    start_date = hmm_states$日期[start_idx],
    end_date   = hmm_states$日期[end_idx],
    state_label = ifelse(state == "1", "低波动区制", "高波动区制")
  )

# 事件窗口
events <- data.frame(
  start = as.Date(c("2025-04-03", "2026-02-28")),
  end   = as.Date(c("2025-04-09", "2026-04-08")),
  event = c("OPEC+增产叠加关税冲击", "美以伊冲突")
)

# =========================================================================
# 统一的学术期刊主题
# =========================================================================
theme_academic <- function() {
  theme_bw(base_family = "Times") +
    theme(
      # 米色背景
      panel.background = element_rect(fill = "#F5F5DC"),
      # 粗黑边框
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      # 去除网格线
      panel.grid = element_blank(),
      # 刻度朝内
      axis.ticks.length = unit(-2, "mm"),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      # 文字样式
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(color = "black", size = 11),
      # 白色绘图背景
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      # 图例
      legend.position = "bottom",
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18)
    )
}

# =========================================================================
# 图1：总溢出指数 (TCI)
# =========================================================================
colnames(tci_df) <- c("日期", "总溢出指数")
# 只改 geom_text 部分，让文字向中间靠拢
p_tci <- ggplot() +
  geom_rect(
    data = subset(state_segments, state_label == "高波动区制"),
    aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
    fill = "gray80", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_rect(
    data = events,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "gray60", alpha = 0.25, inherit.aes = FALSE
  ) +
  # 事件名称标注（修正位置）
  geom_text(
    data = events,
    aes(x = start + (end - start)/2, 
        y = Inf, 
        label = event),
    vjust = -0.5,          # 2.5 → -0.5，文字放在图形内
    size = 5, 
    family = "SimHei",     # Times → SimHei，确保中文显示
    color = "black"
  ) +
  geom_line(
    data = tci_df, 
    aes(x = 日期, y = 总溢出指数),
    color = "black", linewidth = 0.8
  ) +
  scale_x_date(
    date_breaks = "3 months", 
    date_labels = "%Y-%m",
    expand = c(0.02, 0.02)  # 0.01 → 0.02，增加左右留白
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))  # 顶部留出12%空白
  ) +
  labs(x = NULL, y = "总溢出指数 (%)") +
  theme_academic() +
  theme(
    axis.title.y = element_text(size = 14, family = "SimHei"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 5, r = 15, b = 5, l = 10)  # 增加右边距
  )

print(p_tci)
ggsave("plot_TCI_academic.png", p_tci, width = 12, height = 5.5, dpi = 400, bg = "white")
ggsave("plot_TCI_academic.pdf", p_tci, width = 12, height = 5.5, device = cairo_pdf)

# =========================================================================
# 图2：净溢出指数 (NET)
# =========================================================================
net_long <- net_df %>%
  pivot_longer(-日期, names_to = "行业", values_to = "净溢出") %>%
  mutate(
    行业 = factor(行业, levels = c("石油开采", "炼油", "油品销售"))
  )

# 定义黑白友好的线型
line_types <- c("石油开采" = "solid", "炼油" = "dashed", "油品销售" = "dotted")

p_net <- ggplot() +
  geom_rect(
    data = subset(state_segments, state_label == "高波动区制"),
    aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
    fill = "gray80", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_rect(
    data = events,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "gray60", alpha = 0.25, inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.4) +
  geom_line(
    data = net_long,
    aes(x = 日期, y = 净溢出, linetype = 行业),
    color = "black", linewidth = 0.8
  ) +
  scale_linetype_manual(
    values = line_types,
    name = "行业",
    labels = c("石油开采", "炼油", "油品销售")
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%Y-%m",
    expand = c(0.01, 0.01)
  ) +
  labs(
    # title = "各行业净溢出指数 (NET = TO - FROM)",
    subtitle = "正值：净溢出者 | 负值：净接收者 ",
    x = NULL,
    y = "净溢出指数 (%)"
  ) +
  theme_academic() +
  theme(axis.title.y = element_text(size = 14, family = "SimHei"),
      axis.text.x = element_text(angle = 45, hjust = 1))

print(p_net)
ggsave("plot_NET_academic.png", p_net, width = 10, height = 5.5, dpi = 400, bg = "white")
ggsave("plot_NET_academic.pdf", p_net, width = 10, height = 5.5, device = cairo_pdf)

# =========================================================================
# 图3：方向性溢出 (TO vs FROM) - 三个独立图
# =========================================================================
to_long <- to_df %>%
  pivot_longer(-日期, names_to = "行业", values_to = "TO")
from_long <- from_df %>%
  pivot_longer(-日期, names_to = "行业", values_to = "FROM")

direction_df <- to_long %>%
  left_join(from_long, by = c("日期", "行业")) %>%
  pivot_longer(cols = c(TO, FROM), names_to = "方向", values_to = "值") %>%
  mutate(
    行业 = factor(行业, levels = c("石油开采", "炼油", "油品销售")),
    方向 = factor(方向, levels = c("TO", "FROM"))
  )

# 定义三个行业
industries <- c("石油开采", "炼油", "油品销售")

for (ind in industries) {
  
  p_ind <- ggplot() +
    # 高波动区制背景
    geom_rect(
      data = subset(state_segments, state_label == "高波动区制"),
      aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
      fill = "gray80", alpha = 0.5, inherit.aes = FALSE
    ) +
    # 事件窗口
    geom_rect(
      data = events,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
      fill = "gray60", alpha = 0.25, inherit.aes = FALSE
    ) +
    geom_line(
      data = direction_df %>% filter(行业 == ind),
      aes(x = 日期, y = 值, linetype = 方向),
      color = "black", linewidth = 0.8
    ) +
    scale_linetype_manual(
      values = c("TO" = "solid", "FROM" = "dashed"),
      labels = c("TO" = "溢出至其他", "FROM" = "溢入自其他")
    ) +
    scale_x_date(
      date_breaks = "4 months",
      date_labels = "%Y-%m",
      expand = c(0.01, 0.01)
    ) +
    labs(
      x = NULL,
      y = "溢出指数 (%)"
    ) +
    theme_academic() +
    theme(
      axis.title.y = element_text(size = 14, family = "SimHei"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      legend.position = "bottom"
    )
  
  print(p_ind)
  
  # 保存图片
  file_name <- paste0("plot_direction_", 
                      gsub(" ", "_", ind), 
                      "_academic.png")
  ggsave(file_name, p_ind, width = 10, height = 4, dpi = 400, bg = "white")
}

# =========================================================================
# 图4：图例说明（高波动区制背景含义）
# =========================================================================
cat("\n========== 图例说明 ==========\n")
cat("灰色背景区域：HMM 识别的高波动区制\n")
cat("更深的灰色竖条：地缘政治事件窗口\n")
cat("线型说明：\n")
cat("  - 石油开采：实线 (solid)\n")
cat("  - 炼油：虚线 (dashed)\n")
cat("  - 油品销售：点线 (dotted)\n")
cat("  - TO：实线 | FROM：虚线\n")

cat("\n图片已保存：\n")
cat("  - plot_TCI_academic.png / .pdf\n")
cat("  - plot_NET_academic.png / .pdf\n")
cat("  - plot_direction_academic.png / .pdf\n")