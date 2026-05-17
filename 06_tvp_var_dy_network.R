# =========================================================================
# 石油产业链波动溢出网络图（基于真实静态溢出矩阵）
# =========================================================================
library(igraph)

# 安装并加载 showtext 包（用于中文显示）
if (!require("showtext")) {
  install.packages("showtext")
  library(showtext)
} else {
  library(showtext)
}

# 启用 showtext，指定中文字体
font_add("SimHei", "simhei.ttf")
font_add("SimSun", "simsun.ttc")
showtext_auto()

# 1. 读取真实的静态溢出矩阵 ------------------------------------------------
static_table <- read.csv("static_spillover_matrix.csv", row.names = 1)

# 提取三行业的成对溢出子矩阵（排除自身贡献和 FROM/TO/NET 列）
industries <- c("石油开采", "炼油", "油品销售")
pairwise <- as.matrix(static_table[industries, industries])

print("成对溢出矩阵（行：溢出源，列：接收者）：")
print(pairwise)

# 2. 计算节点大小（基于总影响力 = TO + FROM）-------------------------------
TO <- colSums(pairwise) - diag(pairwise)
FROM <- rowSums(pairwise) - diag(pairwise)
Total_Influence <- TO + FROM

# 缩放节点大小以适应图形（根据实际数值调整缩放系数）
node_sizes <- 10 + Total_Influence * 0.8
names(node_sizes) <- rownames(pairwise)

cat("\n各行业总影响力（TO+FROM）：\n")
print(Total_Influence)

# 3. 创建有向图 ------------------------------------------------------------
g <- graph_from_adjacency_matrix(
  pairwise,
  mode = "directed",
  weighted = TRUE,
  diag = FALSE
)

# 4. 设置布局 --------------------------------------------------------------
set.seed(42)
layout <- layout_with_fr(g, niter = 1000)

print("布局坐标：")
print(layout)

# 5. 计算边宽度 ------------------------------------------------------------
edge_weights <- E(g)$weight
edge_width <- 1.5 + (edge_weights / max(edge_weights)) * 5

# 6. 绘制网络图（PNG格式）---------------------------------------------------
png("spillover_network_CN.png", width = 8, height = 6, units = "in", 
    res = 300, bg = "#F5F0E6")

showtext_begin()

par(mar = c(1, 1, 3, 1), bg = "#F5F0E6")

plot(g,
     layout = layout,
     vertex.size = node_sizes,
     vertex.color = "#FFF8DC",
     vertex.frame.color = "#1A1A1A",
     vertex.frame.width = 2.5,
     vertex.label = V(g)$name,
     vertex.label.font = 2,
     vertex.label.cex = 1.5,
     vertex.label.color = "#1A1A1A",
     vertex.label.family = "SimHei",
     edge.width = edge_width,
     edge.color = "#333333",
     edge.arrow.size = 1.2,
     edge.arrow.width = 1.2,
     edge.curved = 0.15,
     edge.label = round(edge_weights, 1),
     edge.label.cex = 1.0,
     edge.label.color = "#333333",
     edge.label.family = "SimHei",
     main = "")

title("石油产业链波动溢出网络", cex.main = 1.8, font.main = 2, family = "SimHei")

# 图例
legend("topleft",
       legend = c(
         sprintf("石油开采: TO=%.1f, FROM=%.1f", TO[1], FROM[1]),
         sprintf("炼油:     TO=%.1f, FROM=%.1f", TO[2], FROM[2]),
         sprintf("油品销售: TO=%.1f, FROM=%.1f", TO[3], FROM[3]),
         "",
         "节点大小 ∝ 总影响力 (TO+FROM)",
         "箭头粗细 ∝ 溢出强度"
       ),
       bty = "n", cex = 1.0, text.col = "#1A1A1A")

showtext_end()
dev.off()

cat("\n网络图已保存至：spillover_network_CN.png\n")

# 7. 保存为 PDF（矢量格式）--------------------------------------------------
pdf("spillover_network_CN.pdf", width = 8, height = 6, bg = "#F5F0E6")

showtext_begin()

par(mar = c(1, 1, 3, 1), bg = "#F5F0E6")

plot(g,
     layout = layout,
     vertex.size = node_sizes,
     vertex.color = "#FFF8DC",
     vertex.frame.color = "#1A1A1A",
     vertex.frame.width = 2.5,
     vertex.label = V(g)$name,
     vertex.label.font = 2,
     vertex.label.cex = 1.5,
     vertex.label.color = "#1A1A1A",
     vertex.label.family = "SimHei",
     edge.width = edge_width,
     edge.color = "#333333",
     edge.arrow.size = 1.2,
     edge.arrow.width = 1.2,
     edge.curved = 0.15,
     edge.label = round(edge_weights, 1),
     edge.label.cex = 1.0,
     edge.label.color = "#333333",
     edge.label.family = "SimHei",
     main = "")

title("石油产业链波动溢出网络", cex.main = 1.8, font.main = 2, family = "SimHei")

legend("topleft",
       legend = c(
         sprintf("石油开采: TO=%.1f, FROM=%.1f", TO[1], FROM[1]),
         sprintf("炼油:     TO=%.1f, FROM=%.1f", TO[2], FROM[2]),
         sprintf("油品销售: TO=%.1f, FROM=%.1f", TO[3], FROM[3]),
         "",
         "节点大小 ∝ 总影响力 (TO+FROM)",
         "箭头粗细 ∝ 溢出强度"
       ),
       bty = "n", cex = 1.0, text.col = "#1A1A1A")

showtext_end()
dev.off()

cat("矢量版本已保存至：spillover_network_CN.pdf\n")