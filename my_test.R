# 均值检验
# 输入：df，需要做检验的自变量（结果的行数），需要做检验的因变量（结果的列数），检验的方法
# 输出：len(x) * len(y) 的df，每一列都是p值
# 方法：自变量是类别型，因变量的类型区分不同方法
#   因变量是数值：
#      两类
#          t: t检验(2类+正态)
#          wilcoxon: wilcoxon检验(2类+非正态)       默认
#      多类
#          anova: 方差分析(多类+正态)
#          kruskal: Kruskal-Wallis检验（多类+非正态）  默认
#   因变量是类别
#      40样本以上
#          卡方检验
#      40样本以下
#          fisher精确检验
#

# 注意：因变量是类别的未测试
x_label_y_diff_check <- function(df, x_cols, y_cols, envs=new.env(), extra_plot=NULL) {
  # 1. 类型校验
  #   x_cols 必须都是factor
  if (!all(sapply(df[x_cols], is.factor))) {
    stop("All x_cols must be factors")
  }
  
  # 2. 初始化
  p_m <- matrix(NA, nrow=length(x_cols), ncol=length(y_cols), dimnames=list(x_cols, y_cols))
  method_m <- matrix(NA, nrow=length(x_cols), ncol=length(y_cols), dimnames=list(x_cols, y_cols))
  
  # 3. 检验
  for (x_col in x_cols) {
    levels_num <- length(levels(df$x_cols))
    default_method <- ifelse(levels_num == 2, "wilcoxon", "kruskal")
    test_method <- get0(x_col, envir=envs, ifnotfound = default_method)
    for (y_col in y_cols) {
      # 如果y是类别型变量，直接根据样本量，设定测试方法
      if (is.factor(df[[y_col]])) {
        if (length(df[[y_cols]]) > 40) {
          test_method <- "anova"
        } else {
          test_method <- "kruskal"
        }
      }
      
      if (test_method == "t") {
        test_result <- t.test(df[[y_col]] ~ df[[x_col]])$p.value
      } else if (test_method == "wilcoxon") {
        test_result <- wilcox.test(df[[y_col]] ~ df[[x_col]])$p.value
      } else if (test_method == "anova") {
        test_result <- aov(df[[y_col]] ~ df[[x_col]])$p.value
      } else if (test_method == "kruskal") {
        test_result <- kruskal.test(df[[y_col]] ~ df[[x_col]])$p.value
      } else {
        stop(paste("Unknown test methodL ", test_method))
      }
      p_m[x_col, y_col] <- test_result
      method_m[x_col, y_col] <- test_method
    }
    
  }
  
  # 4. 画图
  plot_x_cols = c()
  for (x_col in x_cols) {
    for (y_col in y_cols) {
      if (p_m[x_col, y_col] < 0.05) {
        plot_x_cols = append(plot_x_cols, x_col)
        break
      }
    }
  }
  if (!is.null(extra_plot)) {
    for (extra in extra_plot) {
      plot_x_cols = append(plot_x_cols, extra)
    }
  }
  par(mfrow=c(length(plot_x_cols), length(y_cols)))
  for (x_col in plot_x_cols) {
    for (y_col in y_cols) {
      tmp_df = na.omit(df[, c(y_col, x_col)])
      p_value <- p_m[x_col, y_col]
      color <- ifelse(p_value < 0.05, "red", "black")
      # tmp_df = na.omit(data.frame(y_col=df[[y_col]], x_col=df[[x_col]]))
      boxplot(df[[y_col]] ~ df[[x_col]], xlab=NA, ylab=NA)
      title(main=paste("p: ", p_value), col.main=color)
    }
  }
  
  for (row_i in 1:length(plot_x_cols)) {
    mtext(plot_x_cols[row_i], side=2, line=-1.5, outer=TRUE, at=(length(plot_x_cols) - row_i + 0.5) / length(plot_x_cols))
  }
  
  for (col_i in 1:length(y_cols)) {
    mtext(y_cols[col_i], side=3, line=-1.5, outer=TRUE, at=(col_i - 0.5) / length(y_cols))
  }
  
  #dev.off()
  
  # 5. 返回
  return(list("p_df"=as.data.frame(p_m), "method_df"=as.data.frame(method_m)))
}



# 相关性检验
# 输入：df，需要做检验的自变量（结果的行数），需要做检验的因变量（结果的列数），检验的方法
# 输出：len(x) * len(y) 的df，len(x) * len(y) 的df
#       第一个df是相关性，第二个df是p值
# 方法：自变量是数值型，因变量的类型区分不同方法
#   因变量是数值
#     Pearson积差相关系数
#   因变量是有序类别   
#     Spearman等级相关系数
#
x_num_y_cor_check <- function(df, x_cols, y_cols, extra_plot=NULL) {
  # 1. 类型校验
  #   x_cols 必须都是numeric
  if (!all(sapply(df[x_cols], is.numeric))) {
    stop("All x_cols must be numeric")
  }
  
  # 2. 初始化
  cor_m <- matrix(NA, nrow=length(x_cols), ncol=length(y_cols), dimnames=list(x_cols, y_cols))
  p_m <- matrix(NA, nrow=length(x_cols), ncol=length(y_cols), dimnames=list(x_cols, y_cols))
  
  # 3. 相关关系
  for (x_col in x_cols) {
    for (y_col in y_cols) {
      # 如果y是类别型变量，直接根据样本量，设定测试方法
      if (is.factor(df[[y_col]])) {
        cor_method <- "spearman"
      } else if (is.numeric(df[[y_col]])) {
        cor_method <- "pearson"
      } else {
        stop(paste("unsupported y_col: ", y_col))
      }
      
      cor_result <- cor.test(df[[y_col]], df[[x_col]])  
      cor_m[x_col, y_col] <- cor_result$estimate
      p_m[x_col, y_col] <- cor_result$p.value
      
    }
  }
  
  # 4. 画图
  # 只画相关关系显著的图（如果某个x和某个y相关关系显著，那么这个x和所有y的图都画）
  plot_x_cols = c()
  for (x_col in x_cols) {
    for (y_col in y_cols) {
      if (p_m[x_col, y_col] < 0.05) {
        plot_x_cols = append(plot_x_cols, x_col)
        break
      }
    }
  }
  if (!is.null(extra_plot)) {
    for (extra in extra_plot) {
      plot_x_cols = append(plot_x_cols, extra)
    }
  }
  par(mfrow=c(length(plot_x_cols), length(y_cols)))
  for (x_col in plot_x_cols) {
    for (y_col in y_cols) {
      tmp_df = na.omit(df[, c(y_col, x_col)])
      plot(tmp_df[[x_col]], tmp_df[[y_col]], xlab=NA, ylab=NA)
      color <- ifelse(p_m[x_col, y_col] < 0.05, "red", "black")
      title(main=paste("cor=", cor_m[x_col, y_col], ";p=", p_m[x_col, y_col]), col.main=color)
      abline(lm(df[[y_col]]~df[[x_col]]), col="red", lwd=2, lty=1)
      # lines(lowess(df[[x_col]], df[[y_col]]), col="blue", lwd=2, lty=2)
    }
  }
  
  for (row_i in 1:length(plot_x_cols)) {
    mtext(plot_x_cols[row_i], side=2, line=-1.5, outer=TRUE, at=(length(plot_x_cols) - row_i + 0.5) / length(plot_x_cols))
  }
  
  for (col_i in 1:length(y_cols)) {
    mtext(y_cols[col_i], side=3, line=-1.5, outer=TRUE, at=(col_i - 0.5) / length(y_cols))
  }

  
  return(list("cor_df"=as.data.frame(cor_m), "p_df"=as.data.frame(p_m)))
}


# 获取一个df，按照某列正排序后的rownames
# 通常是将一个p值的df传过来，找到让某列p值最小的前n个rownames
get_top_n_x_cols <- function(df, y_col_num, n) {
  y_col = colnames(df)[y_col_num]
  new = df[order(df[[y_col]]), ]
  print(new)
  return(rownames(new)[1:n])
}


