lm_batch <- function (df, x_lm_cols, y_lm_col, threshhold=3) {
  counter <- 0
  start <- Sys.time()
  total <- 2^length(x_lm_cols)
  for (i in 1:length(x_lm_cols)) {
    combinations <- combn(x_lm_cols, i)
    for (j in 1:ncol(combinations)) {
      formula_str = paste(y_lm_col, "~", paste(combinations[, j], collapse=" + "))
      tryCatch({
        model <- lm(formula_str, data=df)
        sig_num <- sum(summary(model)$coefficients[, "Pr(>|t|)"] < 0.05)
        counter = counter + 1
        if (sig_num > threshhold-1) {
          print(paste(sig_num, "/", length(combinations[, j])))
          print(formula_str)
        }
        if (counter %% 10000 == 0){  # 万次。每一万次大概12s
          cost = Sys.time()-start
          print(cost)
          print(paste(counter / 10000, "万 / ", total/10000, '万'))
        }
      }, error=function(e) NULL)
    }
  }
}