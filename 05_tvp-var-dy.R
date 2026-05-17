# =========================================================================
# TVP-VAR-DY 溢出效应分析
# =========================================================================

library(ConnectednessApproach)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)

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

# 4. 提取动态溢出指数 --------------------------------------------------------
burn_in <- 50

tci <- dynamic_result$TCI
tci_df <- data.frame(
  日期 = index(data_zoo)[burn_in:length(index(data_zoo))],
  TCI = tci[burn_in:length(tci)]
)

to_spill <- dynamic_result$TO
to_df <- as.data.frame(to_spill[burn_in:nrow(to_spill), ])
to_df$日期 <- index(data_zoo)[burn_in:length(index(data_zoo))]

from_spill <- dynamic_result$FROM
from_df <- as.data.frame(from_spill[burn_in:nrow(from_spill), ])
from_df$日期 <- index(data_zoo)[burn_in:length(index(data_zoo))]

net_spill <- dynamic_result$NET
net_df <- as.data.frame(net_spill[burn_in:nrow(net_spill), ])
net_df$日期 <- index(data_zoo)[burn_in:length(index(data_zoo))]

npdc <- dynamic_result$NPDC

cat("\n动态溢出指数提取完成，样本量：", nrow(tci_df), "个交易日\n")

# 5. 保存结果 ----------------------------------------------------------------
write.csv(tci_df, "dynamic_TCI.csv", row.names = FALSE)
write.csv(to_df, "dynamic_TO.csv", row.names = FALSE)
write.csv(from_df, "dynamic_FROM.csv", row.names = FALSE)
write.csv(net_df, "dynamic_NET.csv", row.names = FALSE)

cat("\n结果已保存至：\n")
cat("  - static_spillover_matrix.csv\n")
cat("  - dynamic_TCI.csv\n")
cat("  - dynamic_TO.csv\n")
cat("  - dynamic_FROM.csv\n")
cat("  - dynamic_NET.csv\n")