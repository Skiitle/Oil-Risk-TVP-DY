# =========================================================================
# PCA + 综合得分 + fHMM 状态识别（修正版）
# =========================================================================
library(fHMM)
library(dplyr)
library(readr)

# 1. 读取收益率数据 ---------------------------------------------------------
returns <- read.csv("cleaned_returns.csv")
returns$日期 <- as.Date(returns$日期)

ret_matrix <- returns[, c("石油开采", "炼油", "油品销售")]

# 2. 主成分分析 -------------------------------------------------------------
pca <- prcomp(ret_matrix, center = TRUE, scale. = TRUE)

# 查看各主成分方差贡献率
pca_summary <- summary(pca)
print(pca_summary)

# 计算累计方差贡献率
cum_var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
cat("\n累计方差贡献率：\n")
print(cum_var)

# 设定累计贡献率阈值（默认85%）
threshold <- 0.85
n_components <- which(cum_var >= threshold)[1]
cat(sprintf("\n保留前 %d 个主成分（累计贡献率 %.2f%% >= %.0f%%）\n", 
            n_components, cum_var[n_components] * 100, threshold * 100))

# 3. 提取主成分得分并添加到数据框 -------------------------------------------
selected_pcs <- pca$x[, 1:n_components, drop = FALSE]
colnames(selected_pcs) <- paste0("PC", 1:n_components)

# 将各 PC 得分添加到 returns 数据框
for (i in 1:n_components) {
  returns[[paste0("PC", i)]] <- selected_pcs[, i]
}

# 4. 计算综合得分 -----------------------------------------------------------
# 使用方差贡献率作为权重
weights <- pca$sdev[1:n_components]^2 / sum(pca$sdev[1:n_components]^2)
cat("\n各主成分权重（方差贡献率占比）：\n")
for (i in 1:n_components) {
  cat(sprintf("PC%d: %.4f\n", i, weights[i]))
}

# 计算加权综合得分
composite_score <- as.vector(selected_pcs %*% weights)
returns$composite_score <- composite_score

cat(sprintf("\n综合得分 = %.4f * PC1", weights[1]))
if (n_components >= 2) {
  for (i in 2:n_components) {
    cat(sprintf(" + %.4f * PC%d", weights[i], i))
  }
}
cat("\n")

# 5. 对综合得分进行 HMM 状态识别 --------------------------------------------
cat("\n", paste0(rep("=", 50), collapse = ""), "\n")
cat("对综合得分进行 HMM 状态识别\n")
cat(paste0(rep("=", 50), collapse = ""), "\n")

# 准备数据文件
fHMM_composite_data <- data.frame(
  date = returns$日期,
  ret  = returns$composite_score
)

temp_file <- "fhmm_composite_temp.csv"
write_csv(fHMM_composite_data, temp_file)

# 候选状态数
K_candidates <- 2:4
results <- list()

for (k in K_candidates) {
  cat("\n正在拟合 K =", k, "\n")
  
  controls <- set_controls(
    states       = k,
    sdds         = "t",
    file         = temp_file,
    date_column  = "date",
    data_column  = "ret",
    logreturns   = FALSE
  )
  
  data_obj <- try(prepare_data(controls), silent = TRUE)
  if (inherits(data_obj, "try-error")) {
    cat("✗ 数据准备失败\n")
    next
  }
  
  fit <- try(fit_model(data_obj, runs = 50, verbose = FALSE), silent = TRUE)
  
  if (!inherits(fit, "try-error")) {
    results[[as.character(k)]] <- list(
      model = fit,
      AIC   = AIC(fit),
      BIC   = BIC(fit)
    )
    cat(sprintf("✓ 成功：AIC = %.2f, BIC = %.2f\n", AIC(fit), BIC(fit)))
  } else {
    cat("✗ 拟合失败\n")
  }
}

# 6. 选择最优模型并解码状态 ------------------------------------------------
if (length(results) == 0) stop("所有模型均拟合失败。")
best_k <- names(which.min(sapply(results, function(x) x$BIC)))
best_model <- results[[best_k]]$model
cat("\n最优状态数 K =", best_k, "\n")

states_obj <- decode_states(best_model)
states_vec <- states_obj$decoding

returns$state_composite <- factor(states_vec)

# 7. 查看各状态描述统计 -----------------------------------------------------
state_summary <- returns %>%
  group_by(state_composite) %>%
  summarise(
    天数 = n(),
    综合得分均值 = mean(composite_score),
    综合得分标准差 = sd(composite_score),
    .groups = "drop"
  ) %>%
  arrange(综合得分标准差)
print(state_summary)

# 8. 保存结果 ---------------------------------------------------------------
file.remove(temp_file)

# 构建输出列名（确保都存在）
output_cols <- c("日期", "石油开采", "炼油", "油品销售")
if (n_components >= 1) output_cols <- c(output_cols, "PC1")
if (n_components >= 2) output_cols <- c(output_cols, "PC2")
if (n_components >= 3) output_cols <- c(output_cols, "PC3")
output_cols <- c(output_cols, "composite_score", "state_composite")

write.csv(returns[, output_cols], "HMM_states_composite.csv", row.names = FALSE)

cat("\n基于综合得分的 HMM 状态序列已保存至：HMM_states_composite.csv\n")
cat("文件包含以下列：\n")
print(output_cols)

# 9. 可选：与单独使用 PC1 的结果对比 ----------------------------------------
cat("\n", paste0(rep("=", 50), collapse = ""), "\n")
cat("说明：\n")
cat("  - state_composite 是基于综合得分识别的状态\n")
cat("  - 综合得分 = ")
cat(sprintf("%.4f*PC1", weights[1]))
if (n_components >= 2) {
  for (i in 2:n_components) {
    cat(sprintf(" + %.4f*PC%d", weights[i], i))
  }
}
cat("\n  - 标准差较小的状态为低波动区制，较大的为高波动区制\n")