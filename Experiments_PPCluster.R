#パッケージ
library(tidyr)
library(ggplot2)
library(rlist)

# 初期設定
#kFilenames <- c("hayes-roth", "iris", "wine")
kFilenames <- c("hayes-roth")
kIter1 <- 10
kIter2 <- 5
kMerged.rate <- seq(0.1, 0.9,by=0.1)

# MLEM2 実験
results <- list()
source("/home/ooki/R/roughsets/getResults_by_MLEME2.R")
for(fn in kFilenames){
  for(kmr in kMerged.rate){
    sprintf("kmr:%s",kmr)
    results <- list.append(results, getResults_by_MLEM2(kFilenames = fn, 
                                                kIter1 = kIter1, 
                                                kIter2 = kIter2,
                                                kMerged.rate = kmr)
    )
  }
  names(results) <- paste(fn,kMerged.rate,sep="")
}

# accurayを取る
df.results <- data.table(
kmr01 = list.mapv(results[[1]][[1]], precision) %>% unname %>% as.numeric,
kmr02 = list.mapv(results[[2]][[1]], precision) %>% unname %>% as.numeric,
kmr03 = list.mapv(results[[3]][[1]], precision) %>% unname %>% as.numeric,
kmr04 = list.mapv(results[[4]][[1]], precision) %>% unname %>% as.numeric,
kmr05 = list.mapv(results[[5]][[1]], precision) %>% unname %>% as.numeric,
kmr06 = list.mapv(results[[6]][[1]], precision) %>% unname %>% as.numeric,
kmr07 = list.mapv(results[[7]][[1]], precision) %>% unname %>% as.numeric,
kmr08 = list.mapv(results[[8]][[1]], precision) %>% unname %>% as.numeric,
kmr09 = list.mapv(results[[9]][[1]], precision) %>% unname %>% as.numeric
) %>% mutate(fn = names(results[[1]]))

# パレート可視化
df.results %>>%
  gather(var, val, -fn) %>%
  ggplot(aes(x=var, y=val, color=fn)) +
    geom_boxplot()



