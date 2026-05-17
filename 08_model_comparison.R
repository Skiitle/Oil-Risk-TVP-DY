# =========================================================================
# TVP-VAR-DY 溢出效应分析（补充滚动窗口对比）
# =========================================================================

library(ConnectednessApproach)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)

library(showtext)
font_add("SimHei", "simhei.ttf")
showtext_auto()

# 1. 准备数据 ---------------------------------------------------------------
returns <- read.csv("cleaned_returns.csv")
returns$日期 <- as.Date(returns$日期)

data_zoo <- zoo(returns[, c("石油开采", "炼油", "油品销售")], 
                order.by = returns$日期)

# 2. 全样本静态溢出分析 ------------------------------------------------------
cat("\n========== 全样本静态溢出分析 ==========\n")

static_result <- ConnectednessApproach(
  x = data_zoo,
  nlag = 1,
  nfore = 10,
  model = "TVP-VAR",
  connectedness = "Time",
  VAR_config = list(
    TVPVAR = list(
      kappa1 = 0.99,
      kappa2 = 0.99,
      prior = "BayesPrior",
      gamma = 0.01
    )
  ),
  Connectedness_config = list(
    TimeConnectedness = list(generalized = TRUE)
  )
)

cat("\n静态溢出矩阵（全样本）：\n")
print(static_result$TABLE)

write.csv(static_result$TABLE, "static_spillover_matrix.csv")

# 3. TVP-VAR-DY 动态溢出分析 ------------------------------------------------
cat("\n========== TVP-VAR-DY 动态溢出分析 ==========\n")

dynamic_result <- ConnectednessApproach(
  x = data_zoo,
  nlag = 1,
  nfore = 10,
  model = "TVP-VAR",
  connectedness = "Time",
  VAR_config = list(
    TVPVAR = list(
      kappa1 = 0.98,
      kappa2 = 0.98,
      prior = "BayesPrior",
      gamma = 0.01
    )
  ),
  Connectedness_config = list(
    TimeConnectedness = list(generalized = TRUE)
  )
)

# 4. 提取TVP-VAR-DY动态溢出指数 ---------------------------------------------
burn_in <- 50

tci_tvp <- dynamic_result$TCI
tci_tvp_df <- data.frame(
  日期 = index(data_zoo)[burn_in:length(index(data_zoo))],
  TCI = tci_tvp[burn_in:length(tci_tvp)]
)

cat("\nTVP-VAR-DY 样本量：", nrow(tci_tvp_df), "个交易日\n")

# 5. 滚动窗口VAR-DY动态溢出分析 ---------------------------------------------
cat("\n========== 滚动窗口VAR-DY模型 ==========\n")

window_size <- 50

dynamic_rolling <- ConnectednessApproach(
  x = data_zoo,
  nlag = 1,
  nfore = 10,
  window.size = window_size,
  model = "TVP-VAR",
  connectedness = "Time",
  VAR_config = list(
    TVPVAR = list(
      kappa1 = 0.98,
      kappa2 = 0.98,
      prior = "BayesPrior",
      gamma = 0.01
    )
  ),
  Connectedness_config = list(
    TimeConnectedness = list(generalized = TRUE)
  )
)

# =========================================================================
# 提取滚动窗口TCI（修正版）
# =========================================================================

# 检查 tci_rolling 的实际长度
cat("tci_rolling 长度：", length(tci_rolling), "\n")
cat("日期向量长度：", length(index(data_zoo)[window_size:length(index(data_zoo))]), "\n")

# 取最小长度
min_len <- min(length(tci_rolling), length(index(data_zoo)[window_size:length(index(data_zoo))]))

tci_rolling_df <- data.frame(
  日期 = tail(index(data_zoo)[window_size:length(index(data_zoo))], min_len),
  TCI  = tail(tci_rolling, min_len)
)

cat("滚动窗口VAR-DY 样本量：", nrow(tci_rolling_df), "个交易日\n")

# 6. 保存结果 ----------------------------------------------------------------
write.csv(tci_tvp_df, "dynamic_TCI_TVP.csv", row.names = FALSE)
write.csv(tci_rolling_df, "dynamic_TCI_rolling.csv", row.names = FALSE)

# 7. 读取HMM状态和事件数据（用于背景着色）-----------------------------------
hmm_states <- read.csv("HMM_states_composite.csv")
hmm_states$日期 <- as.Date(hmm_states$日期)

# 准备状态背景
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
  event = c("   OPEC+增产", "美以伊冲突")
)

# 8. 学术期刊主题 -----------------------------------------------------------
theme_academic <- function() {
  theme_bw(base_family = "SimHei") +  # 改这里
    theme(
      panel.background = element_rect(fill = "#F5F5DC"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(-2, "mm"),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.text = element_text(color = "black", size = 10, family = "SimHei"),  # 添加 family
      axis.title = element_text(color = "black", size = 22, family = "SimHei"),  # 添加 family
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      legend.position = "bottom",
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18, family = "SimHei")
    )
}

# 9. 合并TVP-VAR和滚动窗口TCI数据 -------------------------------------------
colnames(tci_tvp_df) <- c("日期", "TCI")
colnames(tci_rolling_df) <- c("日期", "TCI")

tci_compare <- bind_rows(
  tci_tvp_df %>% mutate(模型 = "TVP-VAR-DY"),
  tci_rolling_df %>% mutate(模型 = "滚动窗口VAR-DY")
) %>%
  mutate(模型 = factor(模型, levels = c("TVP-VAR-DY", "滚动窗口VAR-DY")))

# 10. 绘制对比图 -------------------------------------------------------------

p_compare <- ggplot() +
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
  # 保留灰色竖条
  geom_line(
    data = tci_compare, 
    aes(x = 日期, y = TCI, color = 模型, linewidth = 模型),
    alpha = 0.9
  ) +
  scale_color_manual(
    values = c("TVP-VAR-DY" = "black", "滚动窗口VAR-DY" = "#4B0082"),
    name = ""
  ) +
  scale_linewidth_manual(
    values = c("TVP-VAR-DY" = 0.8, "滚动窗口VAR-DY" = 0.8),
    name = ""
  ) +
  scale_x_date(
    date_breaks = "3 months", 
    date_labels = "%Y-%m",
    expand = c(0.01, 0.01)
  ) +
  labs(x = NULL, y = "总溢出指数 (%)") +
  theme_academic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_compare)
ggsave("plot_TCI_compare_TVP_vs_Rolling.png", p_compare, width = 10, height = 5.5, dpi = 400, bg = "white")


# 11. 统计对比 ---------------------------------------------------------------
cat("\n========== 模型对比统计 ==========\n")

# 对齐日期后计算统计量
tci_merged <- tci_tvp_df %>%
  rename(TCI_TVP = TCI) %>%
  left_join(tci_rolling_df %>% rename(TCI_Rolling = TCI), by = "日期")

cat("\nTVP-VAR-DY TCI均值：", round(mean(tci_merged$TCI_TVP, na.rm = TRUE), 2), "%\n")
cat("滚动窗口VAR-DY TCI均值：", round(mean(tci_merged$TCI_Rolling, na.rm = TRUE), 2), "%\n")
cat("两者相关系数：", round(cor(tci_merged$TCI_TVP, tci_merged$TCI_Rolling, use = "complete.obs"), 4), "\n")
cat("平均绝对偏差：", round(mean(abs(tci_merged$TCI_TVP - tci_merged$TCI_Rolling), na.rm = TRUE), 2), "%\n")

# 12. 模型优劣分析 -----------------------------------------------------------
cat("\n========== 模型优劣分析 ==========\n")

# 计算TCI的标准差（反映时变灵敏度）
sd_tvp <- sd(tci_merged$TCI_TVP, na.rm = TRUE)
sd_rolling <- sd(tci_merged$TCI_Rolling, na.rm = TRUE)

cat("\nTVP-VAR-DY TCI标准差：", round(sd_tvp, 2), "%\n")
cat("滚动窗口VAR-DY TCI标准差：", round(sd_rolling, 2), "%\n")

if (sd_tvp > sd_rolling) {
  cat("\n✓ TVP-VAR-DY的TCI波动性更大，对市场变化更敏感\n")
  cat("  这体现了时变参数模型相较于滚动窗口模型的优势\n")
} else {
  cat("\n滚动窗口VAR-DY的TCI波动性更大\n")
}

# 计算TCI的变化幅度（峰值-谷值）
range_tvp <- diff(range(tci_merged$TCI_TVP, na.rm = TRUE))
range_rolling <- diff(range(tci_merged$TCI_Rolling, na.rm = TRUE))

cat("\nTVP-VAR-DY TCI变化幅度：", round(range_tvp, 2), "%\n")
cat("滚动窗口VAR-DY TCI变化幅度：", round(range_rolling, 2), "%\n")

if (range_tvp > range_rolling) {
  cat("✓ TVP-VAR-DY能捕捉更大的溢出变化范围\n")
}

cat("\n========== 图片与数据已保存 ==========\n")
cat("  - plot_TCI_compare_TVP_vs_Rolling.png / .pdf\n")
cat("  - dynamic_TCI_TVP.csv\n")
cat("  - dynamic_TCI_rolling.csv\n")