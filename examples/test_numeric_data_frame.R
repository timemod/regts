df_weird <- data.frame(a = c("1.123", "aap", NA), b = c("1", "", "jan"), c= 10:12,
                       stringsAsFactors = FALSE)

df_ok <- data.frame(a = c("1.123", "", NA), b = c("1", "  ", "45"), c= 10:12,
                    stringsAsFactors = FALSE)

df2 <- numeric_data_frame(df_weird)

print(df2)
