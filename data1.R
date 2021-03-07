data1 = read.xlsx("data/data1.xlsx", sheet = 1)
df1 = data1[-1, c(1, seq(2, ncol(data1)-2, 3))]
df2 = data1[-1, c(1, seq(3, ncol(data1)-1, 3))]
df3 = data1[-1, c(1, seq(4, ncol(data1), 3))]
names(df2) = names(df1)
names(df3) = names(df1)
df = rbind(df1, df2, df3) %>%
  mutate(sex = rep(c("total", "m", "f"), each = nrow(df1)))
df[, 2:(ncol(df)-1)] <- apply(df[, 2:(ncol(df)-1)], 2, as.numeric)
df <- df[, c(1, ncol(df), 2:(ncol(df)-1))]
write.xlsx(df, "data/xizang.xlsx")
class(df$合计)
