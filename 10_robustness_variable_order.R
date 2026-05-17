# =========================================================================
# HMM 与 TVP-VAR-DY 稳健性检验（含变量顺序稳健性检验）
# 说明：该脚本不改动原有流程，单独输出稳健性结果文件
# =========================================================================

required_pkgs <- c("ConnectednessApproach", "zoo", "dplyr", "readr", "fHMM")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(sprintf("缺少依赖包：%s。请先安装后再运行。", paste(missing_pkgs, collapse = ", ")))
}

library(ConnectednessApproach)
library(zoo)
library(dplyr)
library(readr)
library(fHMM)

# 1. 数据读取与基础序列构造 -------------------------------------------------
returns <- read.csv("cleaned_returns.csv")
returns$日期 <- as.Date(returns$日期)

industry_cols <- c("石油开采", "炼油", "油品销售")
ret_matrix <- returns[, industry_cols]

pca <- prcomp(ret_matrix, center = TRUE, scale. = TRUE)
pc1 <- pca$x[, 1]

cum_var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
threshold <- 0.85
n_components <- which(cum_var >= threshold)[1]
weights <- pca$sdev[1:n_components]^2 / sum(pca$sdev[1:n_components]^2)
composite_score <- as.vector(pca$x[, 1:n_components, drop = FALSE] %*% weights)

hmm_data <- data.frame(
  date = returns$日期,
  composite = composite_score,
  pc1 = pc1
)

# 2. HMM 稳健性检验 ---------------------------------------------------------
fit_hmm_config <- function(series, dates, k_states, sdds, runs = 50, seed = 20260421) {
  set.seed(seed)
  
  temp_file <- sprintf(
    "temp_fhmm_%s_k%s_%s.csv",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    k_states,
    sdds
  )
  
  write_csv(data.frame(date = dates, ret = series), temp_file)
  on.exit(if (file.exists(temp_file)) file.remove(temp_file), add = TRUE)
  
  controls <- set_controls(
    states = k_states,
    sdds = sdds,
    file = temp_file,
    date_column = "date",
    data_column = "ret",
    logreturns = FALSE
  )
  
  data_obj <- prepare_data(controls)
  fit <- fit_model(data_obj, runs = runs, verbose = FALSE)
  decoded <- decode_states(fit)$decoding
  
  list(
    states = as.integer(decoded),
    AIC = as.numeric(AIC(fit)),
    BIC = as.numeric(BIC(fit))
  )
}

# 用序列波动大小对状态重新排序，确保跨设定可比（1=低波动，最大值=高波动）
relabel_by_vol <- function(state_vec, proxy_series) {
  uniq_states <- sort(unique(state_vec))
  sd_by_state <- sapply(uniq_states, function(s) {
    sd(proxy_series[state_vec == s], na.rm = TRUE)
  })
  ordered_states <- uniq_states[order(sd_by_state)]
  
  rank_map <- setNames(seq_along(ordered_states), ordered_states)
  ordered <- unname(rank_map[as.character(state_vec)])
  
  list(
    ordered_state = as.integer(ordered),
    state_sd = sd_by_state
  )
}

mean_duration <- function(binary_state) {
  r <- rle(binary_state)
  mean(r$lengths)
}

switch_rate <- function(binary_state) {
  if (length(binary_state) <= 1) return(NA_real_)
  mean(abs(diff(binary_state)) > 0)
}

safe_jaccard <- function(x, y) {
  den <- sum(x | y)
  if (den == 0) return(NA_real_)
  sum(x & y) / den
}

hmm_grid <- expand.grid(
  input_series = c("composite", "pc1"),
  K = c(2, 3),
  dist = c("t", "normal"),
  stringsAsFactors = FALSE
)

# 基准设定：与现有主流程一致（composite, K=2, t）
benchmark_fit <- fit_hmm_config(
  series = hmm_data$composite,
  dates = hmm_data$date,
  k_states = 2,
  sdds = "t",
  runs = 50
)
benchmark_relabel <- relabel_by_vol(benchmark_fit$states, hmm_data$composite)
benchmark_high <- benchmark_relabel$ordered_state == max(benchmark_relabel$ordered_state)

hmm_summary <- list()
hmm_state_panel <- list()

for (i in seq_len(nrow(hmm_grid))) {
  cfg <- hmm_grid[i, ]
  cfg_id <- sprintf("hmm_%s_k%s_%s", cfg$input_series, cfg$K, cfg$dist)
  
  series_now <- if (cfg$input_series == "composite") hmm_data$composite else hmm_data$pc1
  
  fit_now <- tryCatch(
    fit_hmm_config(
      series = series_now,
      dates = hmm_data$date,
      k_states = cfg$K,
      sdds = cfg$dist,
      runs = 50
    ),
    error = function(e) e
  )
  
  if (inherits(fit_now, "error")) {
    hmm_summary[[length(hmm_summary) + 1]] <- data.frame(
      config_id = cfg_id,
      input_series = cfg$input_series,
      K = cfg$K,
      dist = cfg$dist,
      AIC = NA_real_,
      BIC = NA_real_,
      mean_duration = NA_real_,
      switch_rate = NA_real_,
      agreement_with_benchmark = NA_real_,
      jaccard_with_benchmark = NA_real_,
      status = paste("failed:", fit_now$message),
      stringsAsFactors = FALSE
    )
    next
  }
  
  relabel_now <- relabel_by_vol(fit_now$states, series_now)
  ordered_now <- relabel_now$ordered_state
  high_now <- ordered_now == max(ordered_now)
  
  agree <- if (cfg$K == 2) mean(high_now == benchmark_high) else NA_real_
  jacc <- if (cfg$K == 2) safe_jaccard(high_now, benchmark_high) else NA_real_
  
  hmm_summary[[length(hmm_summary) + 1]] <- data.frame(
    config_id = cfg_id,
    input_series = cfg$input_series,
    K = cfg$K,
    dist = cfg$dist,
    AIC = fit_now$AIC,
    BIC = fit_now$BIC,
    mean_duration = mean_duration(high_now),
    switch_rate = switch_rate(high_now),
    agreement_with_benchmark = agree,
    jaccard_with_benchmark = jacc,
    status = "ok",
    stringsAsFactors = FALSE
  )
  
  hmm_state_panel[[length(hmm_state_panel) + 1]] <- data.frame(
    日期 = hmm_data$date,
    config_id = cfg_id,
    input_series = cfg$input_series,
    K = cfg$K,
    dist = cfg$dist,
    state_ordered = ordered_now,
    high_vol_indicator = as.integer(high_now),
    stringsAsFactors = FALSE
  )
}

hmm_summary_df <- bind_rows(hmm_summary)
hmm_state_panel_df <- bind_rows(hmm_state_panel)

write.csv(hmm_summary_df, "robustness_HMM_summary.csv", row.names = FALSE)
write.csv(hmm_state_panel_df, "robustness_HMM_states_panel.csv", row.names = FALSE)

cat("\nHMM 稳健性检验完成：\n")
cat("  - robustness_HMM_summary.csv\n")
cat("  - robustness_HMM_states_panel.csv\n")

# 3. TVP-VAR-DY 稳健性检验 --------------------------------------------------
data_zoo <- zoo(returns[, industry_cols], order.by = returns$日期)

run_tvpdy <- function(x, nlag, nfore, kappa1, kappa2, gamma, generalized = TRUE, burn_in = 50) {
  fit <- ConnectednessApproach(
    x = x,
    nlag = nlag,
    nfore = nfore,
    model = "TVP-VAR",
    connectedness = "Time",
    VAR_config = list(
      TVPVAR = list(
        kappa1 = kappa1,
        kappa2 = kappa2,
        prior = "BayesPrior",
        gamma = gamma
      )
    ),
    Connectedness_config = list(
      TimeConnectedness = list(generalized = generalized)
    )
  )
  
  tci <- as.numeric(fit$TCI)
  dates <- as.Date(index(x))
  
  start_idx <- if (length(tci) > burn_in) burn_in + 1 else 1
  
  data.frame(
    日期 = dates[start_idx:length(tci)],
    TCI = tci[start_idx:length(tci)]
  )
}

# 3.1 参数稳健性检验（原有部分）
tvp_configs <- data.frame(
  config_id = c(
    "tvp_base",
    "tvp_lag2",
    "tvp_h8",
    "tvp_h12",
    "tvp_kappa96",
    "tvp_gamma005",
    "tvp_orth"
  ),
  nlag = c(1, 2, 1, 1, 1, 1, 1),
  nfore = c(10, 10, 8, 12, 10, 10, 10),
  kappa1 = c(0.99, 0.99, 0.99, 0.99, 0.96, 0.99, 0.99),
  kappa2 = c(0.99, 0.99, 0.99, 0.99, 0.96, 0.99, 0.99),
  gamma = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.05, 0.01),
  generalized = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

tvp_panels <- list()

for (i in seq_len(nrow(tvp_configs))) {
  cfg <- tvp_configs[i, ]
  
  tci_now <- tryCatch(
    run_tvpdy(
      x = data_zoo,
      nlag = cfg$nlag,
      nfore = cfg$nfore,
      kappa1 = cfg$kappa1,
      kappa2 = cfg$kappa2,
      gamma = cfg$gamma,
      generalized = cfg$generalized,
      burn_in = 50
    ),
    error = function(e) e
  )
  
  if (inherits(tci_now, "error")) {
    tvp_panels[[length(tvp_panels) + 1]] <- data.frame(
      日期 = as.Date(character()),
      TCI = numeric(),
      config_id = character(),
      status = character(),
      stringsAsFactors = FALSE
    )
    
    warning(sprintf("配置 %s 运行失败：%s", cfg$config_id, tci_now$message))
    next
  }
  
  tci_now$config_id <- cfg$config_id
  tci_now$status <- "ok"
  tvp_panels[[length(tvp_panels) + 1]] <- tci_now
}

# 3.2 变量顺序稳健性检验（新增部分）
cat("\n开始 TVP-VAR-DY 变量顺序稳健性检验...\n")

# 生成所有可能的变量排列
library(gtools)
var_names <- industry_cols
n_vars <- length(var_names)
all_permutations <- permutations(n_vars, n_vars, var_names)
n_perms <- nrow(all_permutations)

cat(sprintf("  共 %d 种变量排列\n", n_perms))

# 用基准参数对每种排列运行 TVP-VAR-DY
permutation_panels <- list()

for (p in 1:n_perms) {
  perm_vars <- all_permutations[p, ]
  perm_id <- paste0("perm_", paste(perm_vars, collapse = "_"))
  
  cat(sprintf("  运行排列 %d/%d: %s\n", p, n_perms, perm_id))
  
  # 按排列顺序重新组织数据
  data_zoo_perm <- zoo(returns[, perm_vars], order.by = returns$日期)
  
  tci_now <- tryCatch(
    run_tvpdy(
      x = data_zoo_perm,
      nlag = 1,        # 使用基准参数
      nfore = 10,
      kappa1 = 0.99,
      kappa2 = 0.99,
      gamma = 0.01,
      generalized = TRUE,
      burn_in = 50
    ),
    error = function(e) e
  )
  
  if (inherits(tci_now, "error")) {
    permutation_panels[[length(permutation_panels) + 1]] <- data.frame(
      日期 = as.Date(character()),
      TCI = numeric(),
      config_id = character(),
      perm_order = character(),
      status = character(),
      stringsAsFactors = FALSE
    )
    
    warning(sprintf("  排列 %s 运行失败：%s", perm_id, tci_now$message))
    next
  }
  
  tci_now$config_id <- perm_id
  tci_now$perm_order <- paste(perm_vars, collapse = " -> ")
  tci_now$status <- "ok"
  permutation_panels[[length(permutation_panels) + 1]] <- tci_now
}

# 合并所有结果
tvp_panel_df <- bind_rows(tvp_panels)
permutation_panel_df <- bind_rows(permutation_panels)

# 保存变量顺序稳健性检验的 TCI 面板
write.csv(permutation_panel_df, "robustness_TVPVAR_permutation_TCI_panel.csv", row.names = FALSE)

# 3.3 计算基准结果
bench_tci <- tvp_panel_df %>%
  filter(config_id == "tvp_base") %>%
  select(日期, TCI_base = TCI)

# 3.4 参数稳健性汇总
tvp_summary <- list()
for (cfg_id in unique(tvp_panel_df$config_id)) {
  cfg_tci <- tvp_panel_df %>%
    filter(config_id == cfg_id) %>%
    select(日期, TCI_cfg = TCI)
  
  merged <- inner_join(bench_tci, cfg_tci, by = "日期")
  if (nrow(merged) == 0) {
    tvp_summary[[length(tvp_summary) + 1]] <- data.frame(
      config_id = cfg_id,
      corr_with_base = NA_real_,
      mad_with_base = NA_real_,
      mean_diff = NA_real_,
      rel_mean_diff_pct = NA_real_,
      n_overlap = 0,
      stringsAsFactors = FALSE
    )
    next
  }
  
  rel_diff <- ifelse(
    abs(mean(merged$TCI_base)) < 1e-8,
    NA_real_,
    100 * (mean(merged$TCI_cfg) - mean(merged$TCI_base)) / mean(merged$TCI_base)
  )
  
  tvp_summary[[length(tvp_summary) + 1]] <- data.frame(
    config_id = cfg_id,
    corr_with_base = suppressWarnings(cor(merged$TCI_cfg, merged$TCI_base, use = "complete.obs")),
    mad_with_base = mean(abs(merged$TCI_cfg - merged$TCI_base), na.rm = TRUE),
    mean_diff = mean(merged$TCI_cfg - merged$TCI_base, na.rm = TRUE),
    rel_mean_diff_pct = rel_diff,
    n_overlap = nrow(merged),
    stringsAsFactors = FALSE
  )
}

tvp_summary_df <- bind_rows(tvp_summary) %>%
  left_join(tvp_configs, by = "config_id")

write.csv(tvp_summary_df, "robustness_TVPVAR_summary.csv", row.names = FALSE)

# 3.5 变量顺序稳健性汇总
cat("\n生成变量顺序稳健性汇总...\n")

permutation_summary <- list()

# 计算每种排列与基准的对比
for (p in 1:n_perms) {
  perm_vars <- all_permutations[p, ]
  perm_id <- paste0("perm_", paste(perm_vars, collapse = "_"))
  perm_order <- paste(perm_vars, collapse = " -> ")
  
  cfg_tci <- permutation_panel_df %>%
    filter(config_id == perm_id) %>%
    select(日期, TCI_cfg = TCI)
  
  if (nrow(cfg_tci) == 0) {
    permutation_summary[[length(permutation_summary) + 1]] <- data.frame(
      perm_id = perm_id,
      perm_order = perm_order,
      corr_with_base = NA_real_,
      mad_with_base = NA_real_,
      mean_diff = NA_real_,
      rel_mean_diff_pct = NA_real_,
      n_overlap = 0,
      status = "no_data",
      stringsAsFactors = FALSE
    )
    next
  }
  
  merged <- inner_join(bench_tci, cfg_tci, by = "日期")
  
  if (nrow(merged) == 0) {
    permutation_summary[[length(permutation_summary) + 1]] <- data.frame(
      perm_id = perm_id,
      perm_order = perm_order,
      corr_with_base = NA_real_,
      mad_with_base = NA_real_,
      mean_diff = NA_real_,
      rel_mean_diff_pct = NA_real_,
      n_overlap = 0,
      status = "no_overlap",
      stringsAsFactors = FALSE
    )
    next
  }
  
  # Spearman 秩相关系数（更稳健地反映顺序一致性）
  spearman_corr <- suppressWarnings(
    cor(merged$TCI_cfg, merged$TCI_base, method = "spearman", use = "complete.obs")
  )
  
  # Pearson 相关系数
  pearson_corr <- suppressWarnings(
    cor(merged$TCI_cfg, merged$TCI_base, method = "pearson", use = "complete.obs")
  )
  
  # 平均绝对偏差
  mad_val <- mean(abs(merged$TCI_cfg - merged$TCI_base), na.rm = TRUE)
  
  # 均方根误差
  rmse_val <- sqrt(mean((merged$TCI_cfg - merged$TCI_base)^2, na.rm = TRUE))
  
  # 相对平均差异
  rel_diff <- ifelse(
    abs(mean(merged$TCI_base, na.rm = TRUE)) < 1e-8,
    NA_real_,
    100 * (mean(merged$TCI_cfg, na.rm = TRUE) - mean(merged$TCI_base, na.rm = TRUE)) / 
      mean(merged$TCI_base, na.rm = TRUE)
  )
  
  # Kendall 秩相关系数
  kendall_corr <- suppressWarnings(
    cor(merged$TCI_cfg, merged$TCI_base, method = "kendall", use = "complete.obs")
  )
  
  permutation_summary[[length(permutation_summary) + 1]] <- data.frame(
    perm_id = perm_id,
    perm_order = perm_order,
    pearson_corr = pearson_corr,
    spearman_corr = spearman_corr,
    kendall_corr = kendall_corr,
    mad_with_base = mad_val,
    rmse_with_base = rmse_val,
    mean_diff = mean(merged$TCI_cfg - merged$TCI_base, na.rm = TRUE),
    rel_mean_diff_pct = rel_diff,
    n_overlap = nrow(merged),
    status = "ok",
    stringsAsFactors = FALSE
  )
}

permutation_summary_df <- bind_rows(permutation_summary)

# 添加统计摘要
permutation_summary_df <- permutation_summary_df %>%
  mutate(
    # 是否与基准顺序相同
    is_baseline = (perm_order == paste(industry_cols, collapse = " -> ")),
    # 相关系数分类
    corr_strength = case_when(
      is.na(pearson_corr) ~ NA_character_,
      pearson_corr >= 0.99 ~ "几乎一致",
      pearson_corr >= 0.95 ~ "高度相关",
      pearson_corr >= 0.90 ~ "强相关",
      pearson_corr >= 0.80 ~ "中等相关",
      pearson_corr >= 0.70 ~ "弱相关",
      TRUE ~ "低相关"
    )
  )

# 计算汇总统计量
summary_stats <- permutation_summary_df %>%
  filter(status == "ok") %>%
  summarise(
    n_valid = n(),
    mean_pearson = mean(pearson_corr, na.rm = TRUE),
    sd_pearson = sd(pearson_corr, na.rm = TRUE),
    min_pearson = min(pearson_corr, na.rm = TRUE),
    max_pearson = max(pearson_corr, na.rm = TRUE),
    mean_spearman = mean(spearman_corr, na.rm = TRUE),
    sd_spearman = sd(spearman_corr, na.rm = TRUE),
    mean_mad = mean(mad_with_base, na.rm = TRUE),
    sd_mad = sd(mad_with_base, na.rm = TRUE),
    mean_rmse = mean(rmse_with_base, na.rm = TRUE),
    sd_rmse = sd(rmse_with_base, na.rm = TRUE),
    .groups = "drop"
  )

# 保存结果
write.csv(permutation_summary_df, "robustness_TVPVAR_permutation_summary.csv", row.names = FALSE)
write.csv(summary_stats, "robustness_TVPVAR_permutation_summary_stats.csv", row.names = FALSE)

# 3.6 绘制变量顺序稳健性可视化
cat("\n生成变量顺序稳健性可视化图表...\n")

# 提取所有有效的 TCI 序列用于可视化
permutation_wide <- permutation_panel_df %>%
  filter(status == "ok") %>%
  select(日期, config_id, TCI) %>%
  tidyr::pivot_wider(
    id_cols = 日期,
    names_from = config_id,
    values_from = TCI
  )

# 计算每个时点的统计量
tci_stats <- permutation_wide %>%
  rowwise() %>%
  mutate(
    mean_TCI = mean(c_across(-日期), na.rm = TRUE),
    sd_TCI = sd(c_across(-日期), na.rm = TRUE),
    min_TCI = min(c_across(-日期), na.rm = TRUE),
    max_TCI = max(c_across(-日期), na.rm = TRUE),
    range_TCI = max_TCI - min_TCI
  ) %>%
  ungroup() %>%
  left_join(bench_tci, by = "日期")

# 保存 TCI 统计量
write.csv(tci_stats, "robustness_TVPVAR_permutation_TCI_stats.csv", row.names = FALSE)

# 保存参数稳健性结果
write.csv(tvp_panel_df, "robustness_TVPVAR_TCI_panel.csv", row.names = FALSE)

cat("\nTVP-VAR-DY 稳健性检验完成：\n")
cat("  - robustness_TVPVAR_TCI_panel.csv (参数稳健性 TCI 面板)\n")
cat("  - robustness_TVPVAR_summary.csv (参数稳健性汇总)\n")
cat("  - robustness_TVPVAR_permutation_TCI_panel.csv (变量顺序 TCI 面板)\n")
cat("  - robustness_TVPVAR_permutation_summary.csv (变量顺序详细汇总)\n")
cat("  - robustness_TVPVAR_permutation_summary_stats.csv (变量顺序汇总统计)\n")
cat("  - robustness_TVPVAR_permutation_TCI_stats.csv (变量顺序 TCI 统计量)\n")

# 4. 汇总提示 ---------------------------------------------------------------
cat("\n========== 稳健性检验脚本执行完成 ==========")
cat("\n你可以重点查看：")
cat("\n1) HMM：robustness_HMM_summary.csv 的 BIC、状态切换率、与基准一致率")
cat("\n2) TVP-VAR-DY 参数稳健性：robustness_TVPVAR_summary.csv 的相关系数与 MAD")
cat("\n3) TVP-VAR-DY 变量顺序稳健性：")
cat("\n   - robustness_TVPVAR_permutation_summary.csv 的相关系数")
cat("\n   - robustness_TVPVAR_permutation_summary_stats.csv 的汇总统计")
cat("\n   - robustness_TVPVAR_permutation_TCI_stats.csv 的区间范围")
cat("\n   (若所有排列的 TCI 高度相关且 MAD 很小，则结果对变量顺序稳健)")
cat("\n")