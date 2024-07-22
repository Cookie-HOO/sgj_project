# install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest'))
# devtools::install_github('IRkernel/IRkernel')
# IRkernel::installspec(user = FALSE)

install.packages("stringr")
install.packages("car")
install.packages("psych")


library(stringr)  # 数据预处理
library(car)  # 变量间的相关性图
library(psych)  # 相关系数显著性的检验

setwd("D:\\projects\\sgj_project")

############################## 第一部分：数据处理 ##############################
# 1. 读取数据
df <- read.csv("data-SGJ.csv", fileEncoding = "utf8")
df
# 2. 修正数据
## 2.1. 透析开始时间的第三行2121/12/8 -> 2021/12/8
df$`透析开始时间`[3]  # 2121/12/8
df$`透析开始时间`[3] <- '2021/12/8'
df$`透析开始时间`[3]
## 2.2. 去掉缺失值较多的两行
df[c(27, 36), ]  # 这两行缺失值很多
df <- df[-c(27, 36), ]

# 3. 数据处理
## 3.1. 透析龄.月数. 
df$`截止时间` <- as.POSIXct(df$`截止时间`, format="%Y/%m/%d")
df$`透析开始时间` <- as.POSIXct(df$`透析开始时间`, format="%Y/%m/%d")
df$`透析龄.月数.` <- difftime(df$`截止时间`, df$`透析开始时间`, units="days")
df$`透析龄.月数.` <- as.numeric(df$`透析龄.月数.` / 30)

## 3.2. 基础病
df$`高血压` <- grepl("高血压", df$`基础疾病.DM.CV.`)
df$`糖尿病` <- grepl("糖尿病", df$`基础疾病.DM.CV.`)

## 3.3. 降磷药物及剂量
df$dosage <- as.numeric(str_extract(df$`降磷药物`, "\\d+"))

df$`降磷药物及剂量` <- ifelse(df$`降磷药物`=="无", "无", ifelse(df$dosage < 600, paste0(str_extract(df$`降磷药物`, "\\D+"), "-低"),
                                                   ifelse(df$dosage < 1000, paste0(str_extract(df$`降磷药物`, "\\D+"), "-中"),
                                                          paste0(str_extract(df$`降磷药物`, "\\D+"), "-高"))))
df$`降磷药物` <- ifelse(df$`降磷药物`=="无", "无", ifelse(df$dosage < 600, paste0(str_extract(df$`降磷药物`, "\\D+"), ""),
                                                   ifelse(df$dosage < 1000, paste0(str_extract(df$`降磷药物`, "\\D+"), ""),
                                                          paste0(str_extract(df$`降磷药物`, "\\D+"), ""))))
df$`是否用药` <- ifelse(df$`降磷药物`=="无", FALSE, ifelse(df$`降磷药物`=="无记录", NA, ifelse(is.na(df$`降磷药物`), NA, TRUE)))
df$`降磷药物及剂量`
df$`降磷药物`
df$`是否用药`
## 3.4 骨化三醇/阿法骨化醇
df$`有无骨化三醇.阿法骨化醇` <- ifelse(df$`骨化三醇.阿法骨化醇`=="无", FALSE, ifelse(df$`骨化三醇.阿法骨化醇` %in% c("0.25 qd", "0.25 bid"), TRUE, NA))

## 3.5 吸烟
df$`是否吸烟` <- ifelse(df$`吸烟`=="无", FALSE, ifelse(df$`吸烟` == "有", TRUE, NA))

## 3.6 诺欣妥
df$`有无诺欣妥` <- ifelse(df$`诺欣妥`=="无", FALSE, ifelse(is.na(df$`诺欣妥`), NA, TRUE))

## 3.7 西那卡塞
df$`有无西那卡塞` <- ifelse(df$`西那卡塞`=="无", FALSE, ifelse(is.na(df$`西那卡塞`), NA, TRUE))

# 收集x和y列
y_cols <- c("Klotho.31.2.2000pg.ml.", "FGF23.78.5000pg.ml.", "Fetuin.A.0.78.50ng.ml.", "dp.ucMGP.78.125.5000pg.ml.")

x_n_label_cols <- c("降磷药物", "降磷药物及剂量")
x_2_label_cols <- c("性别", "糖尿病", "高血压", "是否用药", "有无骨化三醇.阿法骨化醇", "是否吸烟", "有无诺欣妥", "有无西那卡塞")
x_num_cols <- c("透析龄.月数.", "年龄", "BMI.kg.m2.", "SBP.mmHg.", "DBP.mmHg.", "Hb.115.150g.L.", "网织红细胞百分比.0.5.1.5..", "Cr.41.81μmol.L.", "Urea.3.1.8.8mmol.L.", "UA.155.357μmol.L.", "二氧化碳.23.0.29.0.mmol.L.", "Ca.2.11.2.52mmol.L.", "P.0.85.1.51mmol.L.", "Ca.P乘积", "ipth.14.9.56.86pg.ml.", "X..β2微球蛋白.1.00.3.00mg.L.", "PA.前白蛋白200.400.", "ALB.40.55g.L.", "ALP.50.135U.L.", "CRP.0.10mg.L.", "铁蛋白.24.425ng.ml.", "转铁饱和度.", "TC.0.5.18mmol.L.", "TG.0.1.7mmol.L.", "LDL.C.0.3.37mmol.L.", "kt.v", "URR")
for (label_col in c(x_2_label_cols, x_n_label_cols)) {
  df[[label_col]] = as.factor(df[[label_col]])
}

############################## 第二部分：数据展示 ##############################
# 4. 数据展示
## 4.1 类别型的x
par(mfrow = c(2, 5))
# 画出每一列的直方图
for (col_name in c(x_2_label_cols, x_n_label_cols)) {
  barplot(table(df[[col_name]]), main=col_name)
  # barplot(table(as.factor(df[[col_name]], main = col_name)))
}

## 4.2 数值型的x
length(x_num_cols)  # 27
sapply(df[, x_num_cols], is.numeric)

par(mfrow = c(3, 9))
# 画出每一列的直方图
for (col_name in x_num_cols) {
  hist(df[[col_name]], main = col_name, xlab=NA)
}

## 4.3 y值
par(mfrow = c(2, 2))
# 画出每一列的直方图
for (col_name in y_cols) {
  hist(df[[col_name]], main = col_name)
}
# 画出每一列的箱图
for (col_name in y_cols) {
  boxplot(df[[col_name]],  main = col_name)
}


############################## 第三部分：变量间相关性检验 ##############################
# 5. 变量相关性检验
## 5.1 多个因变量之间的相关性
df_y = df[y_cols]
scatterplotMatrix(df_y, spread=FALSE, smoother.args=list(lty=2), main="Scatter Plot Matrix")  # 画出交叉关系图
corr.test(df_y, use="complete")  # 参数use=的取值可为"pairwise"或"complete"（分别表示对缺失值执行成对删除或行删除）


## 5.2 自变量和因变量之间的关系检验
source("my_test.R")
## 5.2.1 类别自变量和因变量的均值检验
methods_env <- new.env()
for (col in x_2_label_cols) {
  assign(col, "wilcoxon", envir = methods_env)
}
for (col in x_n_label_cols) {
  assign(col, "kruskal", envir = methods_env)
}
diff_result = x_label_y_diff_check(df, c(x_2_label_cols, x_n_label_cols), y_cols, envs=methods_env, extra_plot=c("是否用药", "降磷药物", "降磷药物及剂量"))
diff_result$p_df
diff_result$method_df
x_lm_label_cols <- get_top_n_x_cols(diff_result$p_df, y_col_num = 2, n=3)
x_lm_label_cols

## 5.2.2 数值自变量和因变量的相关性检验
cor_result = x_num_y_cor_check(df, x_num_cols, y_cols)
cor_result$cor_df
cor_result$p_df
x_lm_num_cols <- get_top_n_x_cols(cor_result$p_df, y_col_num = 2, n=15)
x_lm_num_cols


a# 讨论：是否要做？？？去掉可疑的异常值
new_df <- df[-c(13), ]
new_diff_result = x_label_y_diff_check(new_df, c(x_2_label_cols, x_n_label_cols), y_cols, methods, extra_plot=c("是否用药", "降磷药物", "降磷药物及剂量"))
new_cor_result = x_num_y_cor_check(new_df, x_num_cols, y_cols, extra_plot=c("透析龄.月数."))


############################## 第四部分：回归 ##############################
# 自己写的全子集回归
source("my_lm.R")
y_lm_col <- y_cols[2]
y_lm_col
c(x_lm_label_cols, x_lm_num_cols)
lm_batch(df, c(x_lm_label_cols, x_lm_num_cols), y_lm_col, threshhold = 7)

formula_str <- "FGF23.78.5000pg.ml. ~ 糖尿病 + 有无西那卡塞 + BMI.kg.m2. + 透析龄.月数. + 转铁饱和度. + ALP.50.135U.L. + URR + TC.0.5.18mmol.L. + DBP.mmHg. + kt.v + LDL.C.0.3.37mmol.L. + ALB.40.55g.L. + Cr.41.81μmol.L."
fit <- lm(formula_str, data=df)
summary(fit)

# x_n_label_cols <- c("降磷药物", "降磷药物及剂量")
# x_2_label_cols <- c("性别", "糖尿病", "高血压", "是否用药", "有无骨化三醇.阿法骨化醇", "是否吸烟", "有无诺欣妥", "有无西那卡塞")
# x_num_cols <- c("透析龄.月数.", "年龄", "BMI.kg.m2.", "SBP.mmHg.", "DBP.mmHg.", "Hb.115.150g.L.", "网织红细胞百分比.0.5.1.5..", "Cr.41.81μmol.L.", "Urea.3.1.8.8mmol.L.", "UA.155.357μmol.L.", "二氧化碳.23.0.29.0.mmol.L.", "Ca.2.11.2.52mmol.L.", "P.0.85.1.51mmol.L.", "Ca.P乘积", "ipth.14.9.56.86pg.ml.", "X..β2微球蛋白.1.00.3.00mg.L.", "PA.前白蛋白200.400.", "ALB.40.55g.L.", "ALP.50.135U.L.", "CRP.0.10mg.L.", "铁蛋白.24.425ng.ml.", "转铁饱和度.", "TC.0.5.18mmol.L.", "TG.0.1.7mmol.L.", "LDL.C.0.3.37mmol.L.", "kt.v", "URR")

############################## 其他：回归尝试 ##############################
# 变量定义
y_cols
y_lm_col <- y_cols[2]
# 全量
x_lm_cols <- c(x_n_label_cols, x_2_label_cols, x_num_cols)
# 全量 不要 BMI.kg.m2.
x_lm_cols <- c(x_n_label_cols, x_2_label_cols, x_num_cols[x_num_cols != 'BMI.kg.m2.'])
# 第一个因变量的相关的
x_lm_cols <- c( "UA.155.357μmol.L.", "二氧化碳.23.0.29.0.mmol.L.",  "X..β2微球蛋白.1.00.3.00mg.L.")
# 第二个因变量的相关的
x_lm_cols <- c( "透析龄.月数.",  "BMI.kg.m2.", "糖尿病", "是否吸烟")
# 第二个因变量的相关的  不要BMI.kg.m2.
x_lm_cols <- c( "透析龄.月数.", "糖尿病", "是否吸烟")


# 1. 普通回归
fit <- lm(formula, data=na.omit(df[, c(y_lm_col, x_lm_cols)]))
summary(fit)
p_values <- summary(fit)$coefficients[, "Pr(>|t|)"]

# 2. 逐步回归
install.packages("MASS")
library("MASS")
null_model = lm(formula_min, data=df)
full_model = lm(formula_max, data=df)
step_model = stepAIC(null_model, scope=list(lower=null_model, upper=full_model), direction="both")
summary(step_model)

# 3. 全子集回归
par(mfrow=c(1,1))
install.packages("leaps")
library(leaps)  
leaps <-regsubsets(formula, data=df) 
# 瓷砖图
plot(leaps, scale="adjr2") 
# 带直线的图
library(car) 
subsets(leaps, statistic="cp", main="Cp Plot for All Subsets Regression") 
abline(1,1,lty=2,col="red")
# 最佳变量的回归
x_best1 <- c("降磷药物及剂量", "透析龄.月数.", "DBP.mmHg.", "ipth.14.9.56.86pg.ml.", "ALP.50.135U.L.", "CRP.0.10mg.L.", "TG.0.1.7mmol.L.", "LDL.C.0.3.37mmol.L.")
x_lm_cols <- x_best1
y_lm_col <- y_cols[1]
formula <- as.formula(paste(y_lm_col, "~", paste(x_lm_cols, collapse = " + ")))
fit <- lm(formula, data=na.omit(df[, c(y_lm_col, x_lm_cols)]))
summary(fit)

# 4. LASSO回归
install.packages("glmnet")
library("glmnet")
x <- as.matrix(na.omit(df[, c(y_lm_col, x_lm_cols)])[, x_lm_cols])
y <- na.omit(df[, c(y_lm_col, x_lm_cols)])[, y_lm_col]
cv_fit <- cv.glmnet(x, y, alpha=1)
summary(cv_fit)
coef(cv_fit, s=cv_fit$lambda.min)
# LASSO回归的变量
x_lm_cols <- c("X..β2微球蛋白.1.00.3.00mg.L.", "ALB.40.55g.L.", "CRP.0.10mg.L.", "转铁饱和度.", "TG.0.1.7mmol.L.", "kt.v")
x_lm_cols <- c("降磷药物及剂量", "是否吸烟", "BMI.kg.m2.")
y_lm_col <- y_cols[1]
formula <- as.formula(paste(y_lm_col, "~", paste(x_lm_cols, collapse = " + ")))
fit <- lm(formula, data=na.omit(df[, c(y_lm_col, x_lm_cols)]))
summary(fit)
p_values <- summary(fit)$coefficients[, "Pr(>|t|)"]


