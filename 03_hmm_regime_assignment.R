# =========================================================================
# 学术顶刊风格 Regime Assignment 图：综合波动得分 + HMM 体制状态
# =========================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# 设置全局字体
windowsFonts(Times = windowsFont("Times New Roman"))

# 1. 读取数据 ----------------------------------------------------------------
returns <- read.csv("HMM_states_composite.csv")
returns$日期 <- as.Date(returns$日期)

# 计算波动率代理：综合得分的绝对值
returns <- returns %>%
  mutate(
    state = factor(state_composite, levels = c(1, 2), 
                   labels = c("低波动区制", "高波动区制")),
    volatility = abs(composite_score)
  )

# 2. 定义关键事件区间 --------------------------------------------------------
events <- data.frame(
  start      = as.Date(c("2025-04-03", "2026-02-28")),
  end        = as.Date(c("2025-04-09", "2026-04-08")),
  event_name = c("OPEC+增产叠加关税冲击", "美以伊冲突")
)

# 3. 准备背景矩形数据（状态分段）---------------------------------------------
rle_states <- rle(as.character(returns$state))
state_segments <- data.frame(
  start_idx = c(1, 1 + cumsum(rle_states$lengths)[-length(rle_states$lengths)]),
  end_idx   = cumsum(rle_states$lengths),
  state     = rle_states$values
)
state_segments <- state_segments %>%
  mutate(
    start_date = returns$日期[start_idx],
    end_date   = returns$日期[end_idx]
  )

# 定义颜色（顶刊风格：低饱和度）
bg_colors <- c("低波动区制" = "#E8E8E8", "高波动区制" = "#D3D3D3")  # 浅灰色系
bg_alpha  <- c("低波动区制" = 0.5, "高波动区制" = 1.0)

# 4. 顶刊风格主图 ------------------------------------------------------------
p_top <- ggplot() +
  # 背景矩形，区分状态区制
  geom_rect(data = state_segments,
            aes(xmin = start_date, xmax = end_date,
                ymin = -Inf, ymax = Inf, fill = state, alpha = state),
            inherit.aes = FALSE) +
  scale_alpha_manual(values = bg_alpha, guide = "none") +
  scale_fill_manual(values = bg_colors, name = "体制状态") +
  # 波动率曲线
  geom_line(data = returns, aes(x = 日期, y = volatility),
            color = "black", linewidth = 0.8) +
  # 事件标识
  geom_vline(data = events, aes(xintercept = start), 
             color = "black", linetype = "dashed", linewidth = 0.5) +
  geom_vline(data = events, aes(xintercept = end), 
             color = "black", linetype = "dotted", linewidth = 0.5) +
  # 坐标轴
  scale_x_date(date_breaks = "3 months", date_labels = "%Y年%m月") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "波动率 (|综合得分|)") +
  # 顶刊主题
  theme_bw() +
  theme(
    # 背景与边框
    panel.background = element_rect(fill = "#F5F5DC"),  # 米色背景
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    # 刻度
    axis.ticks.length = unit(-2, "mm"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", family = "Times", size = 9),
    axis.text.x = element_blank(),
    axis.title.y = element_text(family = "Times", size = 10),
    # 图例
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.title = element_text(family = "Times", size = 9, face = "bold"),
    legend.text = element_text(family = "Times", size = 8),
    legend.key = element_rect(fill = "white"),
    # 标题
    plot.title = element_text(family = "Times", size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(family = "Times", size = 9, hjust = 0.5),
    plot.margin = margin(t = 5, r = 10, b = 0, l = 10)
  ) 
# 5. 下面板（状态色块条）------------------------------------------------------
p_bottom <- ggplot() +
  geom_rect(data = state_segments,
            aes(xmin = start_date, xmax = end_date,
                ymin = 0, ymax = 1, fill = state),
            color = NA) +
  scale_fill_manual(values = bg_colors, guide = "none") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y年%m月") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "时间", y = NULL) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#F5F5DC"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    axis.ticks.length = unit(-2, "mm"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.y = element_blank(),
    axis.text = element_text(color = "black", family = "Times", size = 9),
    axis.text.y = element_blank(),
    axis.title.x = element_text(family = "Times", size = 10),
    plot.margin = margin(t = 0, r = 10, b = 5, l = 10)
  )

# 6. 组合上下图 --------------------------------------------------------------
combined_plot <- p_top / p_bottom + 
  plot_layout(heights = c(7, 1))

# 打印图表
print(combined_plot)

# 7. 保存图片 -----------------------------------------------------------------
ggsave("regime_assignment_top_journal.png", combined_plot, 
       width = 10, height = 6, dpi = 600, bg = "white")
ggsave("regime_assignment_top_journal.pdf", combined_plot, 
       width = 10, height = 6, device = cairo_pdf)

cat("\n图片已保存（顶刊风格）：\n")
cat("  - regime_assignment_top_journal.png (600 DPI)\n")
cat("  - regime_assignment_top_journal.pdf (矢量格式)\n")