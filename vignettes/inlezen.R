setwd("c:\users\jcvdr\documents\R\regts\vignettes\")
library(regts)
library(zoo)
library(Hmisc)


test <- read.csv("test4.csv", stringsAsFactors = FALSE)
test

colnames(test)
rownames(test)
reeks <- as.regts(test, time_column = 1)
reeks



# getransponeerde csv inlezen
tt <- read.csv("testt.csv", check.names = FALSE)
tt
t <- transpose_df(tt, colname_column = 1)
t
reeks <- as.regts(t, fun = as.numeric)
reeks

tt1 <- read.csv("testt.csv", check.names = FALSE, row.names = 1)
tt1
t1 <- transpose_df(tt1)
t1
reeks1 <- as.regts(t1, fun = as.numeric)
reeks1

# getransponeerd met integer waarden
ttn <- read.csv("testt_numeric.csv", check.names = FALSE)
ttn
tn <- transpose_df(ttn, colname_column = 1)
reeksn <- as.regts(tn, fun = as.numeric)
reeksn

source("regts_anita.R")
#getransponeerd met labels
ttl <- read.csv("testtlabel.csv", check.names = FALSE)
ttl
tl <- transpose_df(ttl, colname_column = 1, label_column = 2)
tl
reeksl <- as.regts(tl, fun = as.numeric)
reeksl

#getransponeerd met labels en namen verwisseld
tl2 <- transpose_df(ttl, colname_column = 2, label_column = 1)
tl2
reeksl2 <- as.regts(tl2, fun = as.numeric)
reeksl2

#getransponeerd met labels, row.names gebruiken
ttl <- read.csv("testtlabel.csv", check.names = FALSE, row.names = 1)
ttl
tl <- transpose_df(ttl, label_column = 1)
tl
reeksl <- as.regts(tl, fun = as.numeric)
reeksl

#getransponeerd met labels en namen verwisseld, labels als namen gebruiken
tl2 <- transpose_df(ttl, colname_column = 1)
tl2
reeksl2 <- as.regts(tl2, fun = as.numeric)
reeksl2

#getransponeerd met labels, tussenliggende lege kolommen
ttl <- read.csv("testtlabel2.csv", check.names = FALSE)
View(ttl)
ttl <- ttl[-c(1,2,4)]
tl <- transpose_df(ttl, colname_column = 1, label_column = 2)
tl
reeksl <- as.regts(tl, fun = as.numeric)
reeksl

#getransponeerd met labels, tussenliggende niet lege kolommen en rijen
ttl <- read.csv("testtlabel3.csv", check.names = FALSE)
View(ttl)
ttl <- ttl[-c(1,2,4)]
ttl <- ttl[-8, ]
View(ttl)
tl <- transpose_df(ttl, colname_column = 1, label_column = 2)
tl
reeksl <- as.regts(tl, fun = as.numeric)
reeksl

# 'gewone'  csv-files

#gewone csv met labels, varnames op rij 1, labels op rij 2
ttl <- read.csv("tstlabel.csv", stringsAsFactors = FALSE, row.names = 1)
ttl
labels <- as.character(ttl[1, ])
labels
ttl <- ttl[-1, ]
ttl

reeksl <- as.regts.data.frame(ttl, fun = as.numeric)
reeksl
ts_labels(reeksl) <- labels
View(reeksl)

#gewone csv met labels, met extra kolommen, varnames op rij 1, labels op rij 2
ttl <- read.csv("tstlabel2.csv", stringsAsFactors = FALSE)
ttl
ttl <- ttl[ ,-1]
ttl
colnames(ttl) <- as.character(ttl[1, ])
ttl
ttl <- ttl[-1, ]
ttl
labels <- as.character(ttl[1, ])
labels
ttl <- ttl[-1, ]
ttl

reeksl <- as.regts.data.frame(ttl, fun = as.numeric)
reeksl
ts_labels(reeksl) <- labels
View(reeksl)



#gewone csv met labels, varnames op rij 1
tti <- read.csv("tstlabel.csv", stringsAsFactors = FALSE)
tti
reeksl3 <- as.regts.anita(tti, fun = as.numeric, columnwise = TRUE,
                         name_row = "colnames", label_row = 1,
                         index_column = 1)
reeksl3

#gewone csv met labels, varnames op rij 2
tti2 <- read.csv("tstlabel2.csv", stringsAsFactors = FALSE)
tti2
reeksl4 <- as.regts.anita(tti2, fun = as.numeric, columnwise = TRUE,
                          name_row = 1, label_row = 2,
                          index_column = 1)
reeksl4

#getransponeerde verschoven csv met extra kolom inlezen
ttest <- as.data.frame(t(test), stringsAsFactors = FALSE)
ttest
colnames(ttest)
rownames(ttest)
dft <- ttest[-c(1:2),]
dft <- dft[-c(1:3)]
dft <- dft[-c(2)]
dft[1,1] <- ""
dft

reeks4 <- as.regts.anita(dft, fun = as.numeric, columnwise = FALSE,
                         name_column = 1, index_row = 1)





colnames(dft)
rownames(dft)

rnames <- as.character(ttest[,4])
rnames <- rnames[4:nrow(ttest)]
rnames
rownames(dft) <- rnames
clnames <- ttest[3,]
clnames <- clnames[6:ncol(ttest)]
clnames
colnames(dft) <- clnames
dft
dftt <- as.data.frame(t(dft), stringsAsFactors = FALSE)
dftt

tst <- as.regts(dft, columnwise = FALSE)
tst

#tweede poging met name_column
ttest <- as.data.frame(t(test), stringsAsFactors = FALSE)
ttest
dft <- ttest[-c(1:2),-c(1:3)]
dft
dft[1,1] <- "variabelenamen"
dft <- dft[-2]
dft

tst <- as.regts(dft, columnwise = FALSE, name_column = 1)
tst
