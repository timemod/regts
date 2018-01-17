library(regts)

x <- regts(1:10, start = "2010Q1")
print(movav(x, from = -1, to = 2))

print(movav(x, from = -1, to = 2, na_pad = FALSE))
