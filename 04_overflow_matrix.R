# =========================================================================
# 计算全样本平均NPDC矩阵
# =========================================================================

# NPDC是3维数组：N×N×T
npdc_full <- dynamic_result$NPDC

# 计算时间维度上的平均值
npdc_mean <- apply(npdc_full, c(1, 2), mean)

# 设置行列名
industry_cols <- c("石油开采", "炼油", "油品销售")
rownames(npdc_mean) <- colnames(npdc_mean) <- industry_cols

# 打印全样本平均NPDC矩阵
cat("\n========== 全样本平均NPDC矩阵 ==========\n")
print(round(npdc_mean, 2))

# =========================================================================
# 全样本平均溢出网络图（基于静态GFEVD + 平均NPDC）
# =========================================================================
library(igraph)
library(showtext)
font_add("SimHei", "simhei.ttf")
showtext_auto()

industry_cols <- c("石油开采", "炼油", "油品销售")

# 1. 从静态GFEVD矩阵提取成对溢出值（边权重）-------------------------------
static_table <- static_result$TABLE
pairwise_gfevd <- matrix(0, 3, 3)
rownames(pairwise_gfevd) <- colnames(pairwise_gfevd) <- industry_cols

for (i in 1:3) {
  for (j in 1:3) {
    if (i != j) {
      pairwise_gfevd[i, j] <- as.numeric(static_table[i, j])
    }
  }
}

cat("\n全样本平均成对溢出（GFEVD）：\n")
print(round(pairwise_gfevd, 2))

# 2. 计算全样本平均NPDC（确定边方向）---------------------------------------
npdc_full <- dynamic_result$NPDC
npdc_mean <- apply(npdc_full, c(1, 2), mean)
rownames(npdc_mean) <- colnames(npdc_mean) <- industry_cols

cat("\n全样本平均NPDC：\n")
print(round(npdc_mean, 2))

# 3. 构建有向加权邻接矩阵-----------------------------------------------
# 规则：边权重 = GFEVD成对溢出值，边方向由NPDC符号决定
build_adjacency_fullsample <- function(gfevd_mat, npdc_mat, threshold = 0.5) {
  n <- nrow(gfevd_mat)
  adj_mat <- matrix(0, n, n)
  rownames(adj_mat) <- colnames(adj_mat) <- rownames(gfevd_mat)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        # 权重 = GFEVD成对溢出值
        weight <- gfevd_mat[i, j]
        
        # 方向由NPDC决定
        if (npdc_mat[i, j] > threshold) {
          adj_mat[i, j] <- weight  # i → j
        } else if (npdc_mat[j, i] > threshold) {
          adj_mat[j, i] <- weight  # j → i
        } else {
          # NPDC接近0时，保留双向边（对称）
          adj_mat[i, j] <- weight
          adj_mat[j, i] <- gfevd_mat[j, i]
        }
      }
    }
  }
  return(adj_mat)
}

adj_full <- build_adjacency_fullsample(pairwise_gfevd, npdc_mean, threshold = 0.1)

cat("\n全样本邻接矩阵：\n")
print(round(adj_full, 2))

# 4. 绘制全样本溢出网络图-----------------------------------------------
plot_network_fullsample <- function(adj_mat, title_str, save_name) {
  g <- graph_from_adjacency_matrix(adj_mat, mode = "directed", weighted = TRUE, diag = FALSE)
  
  # 节点大小 ∝ 总出入度（总影响力）
  node_size <- 45 + (rowSums(adj_mat) + colSums(adj_mat)) * 0.8
  
  # 边宽度 ∝ 权重
  edge_weights <- E(g)$weight
  edge_width <- 1.5 + edge_weights / 8
  
  # 布局
  set.seed(42)
  layout <- layout_with_fr(g, niter = 1000)
  
  # 手动调整布局（可选）
  # layout <- matrix(c(-1, 0.5, 0, -0.5, 1, 0.5), ncol = 2, byrow = TRUE)
  
  png(paste0(save_name, ".png"), width = 8, height = 7, units = "in", res = 300, bg = "#F5F0E6")
  showtext_begin()
  
  par(mar = c(1, 1, 3, 1), bg = "#F5F0E6")
  plot(g,
       layout = layout,
       vertex.size = node_size,
       vertex.color = "#FFF8DC",
       vertex.frame.color = "#1A1A1A",
       vertex.frame.width = 2.5,
       vertex.label = V(g)$name,
       vertex.label.font = 2,
       vertex.label.cex = 1.3,
       vertex.label.color = "#1A1A1A",
       vertex.label.family = "SimHei",
       edge.width = edge_width,
       edge.color = "#333333",
       edge.arrow.size = 1.2,
       edge.arrow.width = 1.2,
       edge.curved = 0.15,
       edge.label = round(edge_weights, 1),
       edge.label.cex = 0.9,
       edge.label.color = "#8B0000",
       edge.label.family = "SimHei",
       main = title_str)
  
  showtext_end()
  dev.off()
  
  cat("已保存：", save_name, ".png\n")
}

plot_network_fullsample(adj_full, 
                        "石油产业链全样本平均溢出网络", 
                        "network_fullsample")