library(Hmisc)
library(regts)

df <- data.frame(namen = c("a", "b", "c"), labels = c("Variabele a",
                                                      "Variabele b",
                                                      "Variabele c"),
                 x = 1:3, y = 10:12)
df2 <- transpose_df(df, colname_column = 1, label_column = 2)
print(df2)

df3 <- transpose_df(df2)
print(df3)

