# =========================================================================
# Granger因果检验：检验事件窗口与状态切换的关系
# =========================================================================
library(lmtest)
library(dplyr)
library(tseries)   # ADF检验

# 1. 准备数据 ---------------------------------------------------------------
returns <- read.csv("cleaned_returns.csv")
returns$日期 <- as.Date(returns$日期)

hmm_states <- read.csv("HMM_states_composite.csv")
hmm_states$日期 <- as.Date(hmm_states$日期)

# 合并数据
data <- returns %>%
  left_join(hmm_states[, c("日期", "state_composite")], by = "日期") %>%
  mutate(
    # 高波动状态指示变量
    high_vol = ifelse(state_composite == 2, 1, 0),
    
    # 美以冲突事件窗口（2026-02-28 至 2026-04-08）
    conflict = ifelse(日期 >= as.Date("2026-02-28") & 
                        日期 <= as.Date("2026-04-08"), 1, 0),
    
    # 年末窗口（12月）
    year_end = ifelse(format(日期, "%m") == "12", 1, 0),
    
    # OPEC+冲击窗口（2025-04-03 至 2025-04-09）
    opec = ifelse(日期 >= as.Date("2025-04-03") & 
                    日期 <= as.Date("2025-04-09"), 1, 0)
  )

# 2. 检验序列平稳性（Granger检验的前提）-------------------------------------
cat("========== ADF平稳性检验 ==========\n")
cat("高波动状态序列:\n")
adf_high <- adf.test(data$high_vol)
cat("  ADF统计量 =", round(adf_high$statistic, 4), "\n")
cat("  p-value =", format.pval(adf_high$p.value, digits = 4), "\n")
cat("  结论:", ifelse(adf_high$p.value < 0.05, "平稳 ✓", "非平稳 ✗"), "\n\n")

cat("冲突事件序列:\n")
adf_conflict <- adf.test(data$conflict)
cat("  ADF统计量 =", round(adf_conflict$statistic, 4), "\n")
cat("  p-value =", format.pval(adf_conflict$p.value, digits = 4), "\n")
cat("  结论:", ifelse(adf_conflict$p.value < 0.05, "平稳 ✓", "非平稳 ✗"), "\n")

# 3. 确定最优滞后阶数 --------------------------------------------------------
library(vars)
var_data <- data[, c("high_vol", "conflict")]
var_select <- VARselect(var_data, lag.max = 10, type = "const")

cat("\n========== 最优滞后阶数选择 ==========\n")
cat("AIC 最优滞后阶数:", var_select$selection["AIC(n)"], "\n")
cat("BIC 最优滞后阶数:", var_select$selection["SC(n)"], "\n")
cat("HQ 最优滞后阶数:", var_select$selection["HQ(n)"], "\n")

# 4. Granger因果检验 ---------------------------------------------------------
optimal_lag <- var_select$selection["SC(n)"]  # 使用BIC选择的最优滞后阶数
cat("\n========== Granger因果检验（滞后", optimal_lag, "阶）==========\n")

# 检验1：冲突事件是否是高波动状态的Granger原因
granger_conflict_to_high <- grangertest(high_vol ~ conflict, 
                                        order = optimal_lag, 
                                        data = data)
cat("\nH0: 冲突事件不是高波动状态的Granger原因\n")
print(granger_conflict_to_high)
cat("结论:", ifelse(granger_conflict_to_high$`Pr(>F)`[2] < 0.05,
                  "拒绝H0，冲突事件显著Granger引起高波动状态 ✓",
                  "不能拒绝H0，冲突事件不显著Granger引起高波动状态"), "\n")

# 检验2：高波动状态是否是冲突事件的Granger原因（反向检验）
granger_high_to_conflict <- grangertest(conflict ~ high_vol, 
                                        order = optimal_lag, 
                                        data = data)
cat("\nH0: 高波动状态不是冲突事件的Granger原因\n")
print(granger_high_to_conflict)
cat("结论:", ifelse(granger_high_to_conflict$`Pr(>F)`[2] < 0.05,
                  "拒绝H0，高波动状态显著Granger引起冲突事件",
                  "不能拒绝H0，高波动状态不显著Granger引起冲突事件"), "\n")

# 5. 分因素Granger检验（年末、OPEC+）-----------------------------------------
cat("\n========== 多因素Granger因果检验 ==========\n")

# 检验年末效应
granger_yearend <- grangertest(high_vol ~ year_end, 
                               order = optimal_lag, 
                               data = data)
cat("\n年末效应 → 高波动状态:\n")
cat("  F统计量 =", round(granger_yearend$F[2], 4), "\n")
cat("  p-value =", format.pval(granger_yearend$`Pr(>F)`[2], digits = 4), "\n")

# 检验OPEC+冲击
granger_opec <- grangertest(high_vol ~ opec, 
                            order = optimal_lag, 
                            data = data)
cat("\nOPEC+冲击 → 高波动状态:\n")
cat("  F统计量 =", round(granger_opec$F[2], 4), "\n")
cat("  p-value =", format.pval(granger_opec$`Pr(>F)`[2], digits = 4), "\n")

# 6. 滚动窗口Granger检验（稳健性）--------------------------------------------
cat("\n========== 滚动窗口Granger检验 ==========\n")
window_size <- 100
n_windows <- nrow(data) - window_size + 1
p_values <- numeric(n_windows)
dates_window <- data$日期[window_size:nrow(data)]

for (i in 1:n_windows) {
  idx <- i:(i + window_size - 1)
  data_window <- data[idx, ]
  test <- try(grangertest(high_vol ~ conflict, order = optimal_lag, data = data_window), 
              silent = TRUE)
  if (!inherits(test, "try-error")) {
    p_values[i] <- test$`Pr(>F)`[2]
  } else {
    p_values[i] <- NA
  }
}

# 统计显著窗口比例
sig_prop <- mean(p_values < 0.05, na.rm = TRUE) * 100
cat("显著窗口比例:", round(sig_prop, 1), "%\n")