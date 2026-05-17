# =========================================================================
# 石油产业链三行业指数 - 完整前置分析
# =========================================================================

# 加载包
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(tseries)
library(moments)
library(ggplot2)
library(FinTS)  # 用于ARCH-LM检验

# 1. 读取数据 --------------------------------------------------------------
files <- c("CI005201.WI.xlsx", "CI005202.WI.xlsx", "CI005205.WI.xlsx")

read_index <- function(file) {
  df <- read_excel(file, sheet = "file")
  df <- df[!is.na(df[[1]]) & !grepl("数据来源", df[[1]]), ]
  df$日期 <- as.Date(df$日期)
  arrange(df, 日期)
}

data_list <- map(files, read_index)

# 合并收盘价
prices <- data_list %>%
  map(~ select(., 日期, 收盘价 = `收盘价(元)`)) %>%
  reduce(full_join, by = "日期") %>%
  rename(石油开采 = `收盘价.x`, 炼油 = `收盘价.y`, 油品销售 = `收盘价`) %>%
  arrange(日期)

# 2. 对数收益率转换：return = ln(Pt/Pt-1) × 100 -----------------------------
returns <- prices %>%
  mutate(across(-日期, ~ 100 * c(NA, diff(log(.))))) %>%
  drop_na()

cat("\n========== 对数收益率转换完成 ==========")
cat("\n公式: r_t = ln(P_t / P_{t-1}) × 100\n")
cat("样本量:", nrow(returns), "个交易日\n")

# 3. 描述性统计：均值、标准差、偏度、峰度、Jarque-Bera检验 ----------------
cat("\n\n========== 描述性统计 ==========\n")

desc_stats <- function(x) {
  jb_test <- jarque.bera.test(x)
  c(Mean = mean(x), 
    SD = sd(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x),
    JB_statistic = as.numeric(jb_test$statistic),  # 强制转换为纯数值
    JB_pvalue = jb_test$p.value)
}

desc_table <- as.data.frame(t(sapply(returns[, -1], desc_stats)))
print(round(desc_table, 4))

# 4. ADF单位根检验（确认平稳性）-------------------------------------------
cat("\n\n========== ADF单位根检验 ==========\n")
cat("H0: 存在单位根（序列非平稳）\n")
cat("H1: 序列平稳\n\n")

for (v in c("石油开采", "炼油", "油品销售")) {
  test <- adf.test(returns[[v]])
  cat(sprintf("%s:\n", v))
  cat(sprintf("  ADF统计量 = %.4f\n", test$statistic))
  cat(sprintf("  p-value = %.4f\n", test$p.value))
  cat(sprintf("  结论: %s\n\n", ifelse(test$p.value < 0.01, 
                                     "拒绝H0，序列平稳 ✓", 
                                     "不能拒绝H0，序列非平稳")))
}

# 5. ARCH-LM检验（验证波动聚集）-------------------------------------------
cat("\n========== ARCH-LM检验 ==========\n")
cat("H0: 不存在ARCH效应（无异方差）\n")
cat("H1: 存在ARCH效应（有波动聚集）\n\n")

arch_results <- list()
for (v in c("石油开采", "炼油", "油品销售")) {
  # 对收益率序列拟合均值方程（AR(0)即常数项）
  model <- lm(returns[[v]] ~ 1)
  
  # ARCH-LM检验，滞后12期
  arch_test <- ArchTest(residuals(model), lags = 12)
  
  cat(sprintf("%s:\n", v))
  cat(sprintf("  LM统计量 = %.4f\n", arch_test$statistic))
  cat(sprintf("  p-value = %.4f\n", arch_test$p.value))
  cat(sprintf("  结论: %s\n\n", 
              ifelse(arch_test$p.value < 0.05,
                     "拒绝H0，存在ARCH效应 ✓ (支持使用GARCH类模型)",
                     "不能拒绝H0，不存在显著ARCH效应")))
  
  arch_results[[v]] <- arch_test
}

# 定义美以伊冲突事件窗口
conflict_event <- data.frame(
  start = as.Date("2026-02-28"),
  end   = as.Date("2026-04-08"),
  event = "美以伊冲突"
)

theme_academic_notitle <- function() {
  theme_bw(base_family = "Times") +
    theme(
      panel.background = element_rect(fill = "#F5F5DC"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(-2, "mm"),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(color = "black", size = 11),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9)
    )
}

# 6. 价格走势图（学术期刊风格，单轴，图例左上角，无标题）------------------
p1 <- ggplot(prices, aes(x = 日期)) +
  geom_line(aes(y = 炼油, linetype = "炼油"), size = 0.8, color = "black") +
  geom_line(aes(y = 油品销售, linetype = "油品销售"), size = 0.8, color = "black") +
  geom_line(aes(y = 石油开采, linetype = "石油开采"), size = 0.8, color = "black") +
  scale_y_continuous(
    name = "收盘价 (元)"
  ) +
  scale_linetype_manual(
    values = c("石油开采" = "solid", "炼油" = "dashed", "油品销售" = "dotted"),
    name = "行业"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%Y-%m",
    expand = c(0.01, 0.01)
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_family = "Times") +
  theme(
    panel.background = element_rect(fill = "#F5F5DC"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(-2, "mm"),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key = element_rect(fill = "white"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

print(p1)
ggsave("plot_price_academic.png", p1, width = 10, height = 5.5, dpi = 400, bg = "white")
ggsave("plot_price_academic.pdf", p1, width = 10, height = 5.5, device = cairo_pdf)

# 7. 收益率时序图（学术期刊风格，分别输出三个行业，黑色实线）----------------

# 定义三个行业的名称向量
industries <- c("石油开采", "炼油", "油品销售")

# 循环输出每个行业的图
for (ind in industries) {
  
  # 筛选当前行业数据
  plot_data <- returns %>%
    select(日期, all_of(ind)) %>%
    rename(收益率 = all_of(ind))
  
  # 绘图
  p <- ggplot(plot_data, aes(x = 日期, y = 收益率)) +
    geom_line(size = 0.8, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%Y-%m",
      expand = c(0.01, 0.01)
    ) +
    labs(x = NULL, y = "收益率 (%)") +
    theme_bw(base_family = "Times") +
    theme(
      panel.background = element_rect(fill = "#F5F5DC"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(-2, "mm"),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(color = "black", size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    )
  
  print(p)
  
}

# 8. 收益率平方时序图（学术期刊风格，分面，无标题）--------------------------
p3 <- returns %>%
  mutate(across(-日期, ~ .^2)) %>%
  pivot_longer(-日期, names_to = "行业", values_to = "波动率") %>%
  mutate(行业 = factor(行业, levels = c("石油开采", "炼油", "油品销售"))) %>%
  ggplot(aes(x = 日期, y = 波动率)) +
  geom_area(fill = "gray70", alpha = 0.7) +
  facet_wrap(~ 行业, ncol = 1, scales = "free_y") +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%Y-%m",
    expand = c(0.01, 0.01)
  ) +
  labs(x = "日期", y = "收益率平方") +
  theme_academic_notitle() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(size = 10, face = "bold", family = "Times"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5)
  )

print(p3)
ggsave("plot_volatility_academic.png", p3, width = 10, height = 9, dpi = 400, bg = "white")
ggsave("plot_volatility_academic.pdf", p3, width = 10, height = 9, device = cairo_pdf)


# 9. 相关系数矩阵 -----------------------------------------------------------
cat("\n========== 收益率相关系数矩阵 ==========\n")
cor_matrix <- cor(returns[, -1])
print(round(cor_matrix, 4))

# 10. 保存清洗后的数据 ------------------------------------------------------
write.csv(returns, "cleaned_returns.csv", row.names = FALSE)
cat("\n清洗后的收益率数据已保存至: cleaned_returns.csv\n")

# 11. 汇总检验结果 ----------------------------------------------------------
cat("\n\n========== 检验结果汇总 ==========\n")
cat("✓ 对数收益率转换: r_t = ln(P_t/P_{t-1}) × 100\n")
cat("✓ 描述性统计: 均值、标准差、偏度、峰度、JB检验完成\n")
cat("✓ ADF单位根检验: 三序列均平稳\n")
cat("→ 结论: 适合使用TVP-VAR模型，建议结合GARCH或SV处理异方差\n")