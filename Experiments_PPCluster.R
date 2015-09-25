#パッケージ
library(tidyr)
library(ggplot2)
library(rlist)

# 初期設定
#kFilenames <- c("hayes-roth", "iris", "wine", "zoo")
rm(list = ls())
kFilenames <- c("wine")
kIter1 <- 10
kIter2 <- 10
kMerged.rate <- seq(0.0, 0.9, by=0.1)
kMerged.size <- seq(0, 18, by=2)
mgs <- 1
#mgs <- "max"
rpm <- "Merged"
#rpm <- "Randomed"

# MLEM2 実験
results <- list()
source("~/R/roughsets/getResults_by_MLEME2.R")

# kMerged.rateの実験
for(fn in kFilenames){
  for(kmr in kMerged.rate){
    sprintf("kmr:%s",kmr)
    results <- list.append(results, getResults_by_MLEM2(kFilenames = fn, 
                                                kIter1 = kIter1, 
                                                kIter2 = kIter2,
                                                kms = 0,
                                                kmr = kmr,
                                                mgs = mgs,
                                                rpm = rpm)
    )
  }
}
names(results) <- c(paste(kFilenames[1], kMerged.rate, sep=""))
#names(results) <- c(paste(kFilenames[1], kMerged.rate, sep=""), 
#                    paste(kFilenames[2], kMerged.rate, sep=""))

# kMerged.sizeの実験
for(fn in kFilenames){
  for(kms in kMerged.size){
    sprintf("kms:%s",kms)
    results <- list.append(results, getResults_by_MLEM2(kFilenames = fn, 
                                                        kIter1 = kIter1, 
                                                        kIter2 = kIter2,
                                                        kms = kms,
                                                        kmr = 0,
                                                        mgs = mgs,
                                                        rpm = rpm)
    )
  }
}
names(results) <- c(paste(kFilenames[1], kMerged.size, sep=""))
#names(results) <- c(paste(kFilenames[1], kMerged.rate, sep=""), 
#                    paste(kFilenames[2], kMerged.rate, sep=""))

# accurayを取る
dt.results <- data.table()
for(n in 1:length(kFilenames)){
  dt.results <- rbind_list(dt.results, data.table(
    kmr00 = list.mapv(results[[10*n - 9]][[1]], precision) %>% unname %>% as.numeric,
    kmr01 = list.mapv(results[[10*n - 8]][[1]], precision) %>% unname %>% as.numeric,
    kmr02 = list.mapv(results[[10*n - 7]][[1]], precision) %>% unname %>% as.numeric,
    kmr03 = list.mapv(results[[10*n - 6]][[1]], precision) %>% unname %>% as.numeric,
    kmr04 = list.mapv(results[[10*n - 5]][[1]], precision) %>% unname %>% as.numeric,
    kmr05 = list.mapv(results[[10*n - 4]][[1]], precision) %>% unname %>% as.numeric,
    kmr06 = list.mapv(results[[10*n - 3]][[1]], precision) %>% unname %>% as.numeric,
    kmr07 = list.mapv(results[[10*n - 2]][[1]], precision) %>% unname %>% as.numeric,
    kmr08 = list.mapv(results[[10*n - 1]][[1]], precision) %>% unname %>% as.numeric,
    #kmr09 = list.mapv(results[[10*n - 0]][[1]], precision) %>% unname %>% as.numeric,
    fn    = names(results[[10*n - 9]]))
  )
}
#setnames(dt.results, c(paste("kms", kMerged.size, sep=""), "fn"))

# entropyを取る
dt.entropy <- data.table()
for(n in 1:length(kFilenames)){
  dt.entropy <- rbind_list(dt.entropy, data.table(
    kmr00 = list.mapv(results[[10*n - 9]][[1]], entropy) %>% unname %>% as.numeric,
    kmr01 = list.mapv(results[[10*n - 8]][[1]], entropy) %>% unname %>% as.numeric,
    kmr02 = list.mapv(results[[10*n - 7]][[1]], entropy) %>% unname %>% as.numeric,
    kmr03 = list.mapv(results[[10*n - 6]][[1]], entropy) %>% unname %>% as.numeric,
    kmr04 = list.mapv(results[[10*n - 5]][[1]], entropy) %>% unname %>% as.numeric,
    kmr05 = list.mapv(results[[10*n - 4]][[1]], entropy) %>% unname %>% as.numeric,
    kmr06 = list.mapv(results[[10*n - 3]][[1]], entropy) %>% unname %>% as.numeric,
    kmr07 = list.mapv(results[[10*n - 2]][[1]], entropy) %>% unname %>% as.numeric,
    kmr08 = list.mapv(results[[10*n - 1]][[1]], entropy) %>% unname %>% as.numeric,
    kmr09 = list.mapv(results[[10*n - 0]][[1]], entropy) %>% unname %>% as.numeric,
    fn    = names(results[[10*n - 9]]))
  )
}
#setnames(dt.entropy, c(paste("kms", kMerged.size, sep=""), "fn"))

# パレート可視化
dt.results %>>%
  gather(var, val, -fn) %>%
  ggplot() +
    geom_boxplot(aes(x=var, y=val, color=fn)) +
    geom_line(data = dt.results %>% gather(var, val, -fn) %>% group_by(fn,var) %>% summarise(m = mean(val)), 
              aes(x=var, y=m, group=1, color=fn), size=rev(0.8)) +
    geom_point(data = dt.results %>% gather(var, val, -fn) %>% group_by(fn,var) %>% summarise(m = mean(val)), 
               aes(x=var, y=m, group=1, color=fn), shape=21, size=1.5) +
    facet_grid(fn~.) + 
  　scale_y_continuous(breaks=seq(0.0, 1.0, by=0.1))
  
# パレート Entropy 可視化
dt.entropy %>>%
  gather(var, val, -fn) %>%
  ggplot() +
  geom_boxplot(aes(x=var, y=val, color=fn)) +
  geom_line(data = dt.entropy %>% gather(var, val, -fn) %>% group_by(fn,var) %>% summarise(m = mean(val)), 
            aes(x=var, y=m, group=1, color=fn), size=rev(0.8)) +
  geom_point(data = dt.entropy %>% gather(var, val, -fn) %>% group_by(fn,var) %>% summarise(m = mean(val)), 
             aes(x=var, y=m, group=1, color=fn), shape=21, size=1.5) +
  facet_grid(fn~.)


