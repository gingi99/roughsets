## MLEM2によるルール抽出による推定結果を返す関数
getResults_by_MLEM2 <- function(kFilenames = c("iris"), 
                                kIter1 = 2,
                                kIter2 = 5,
                                kMerged.rate = 0.1){
  kIter  <- kIter1*kIter2
  results <- list()
  for(fn in kFilenames){
    results.fn <- list()
    for(i in 1:kIter1){
      for(j in 1:kIter2){
        print(paste("fn:",fn," i:", i," j:",j, sep=""))
        
        data.train <- read_tsv(paste("/data/uci/",fn,"/",fn,"-train",i,"-",j,".tsv", sep=""))
        data.train <- na.omit(data.train)
        data.test  <- read_tsv(paste("/data/uci/",fn,"/",fn,"-test",i,"-",j,".tsv", sep=""))
        data.test  <- na.omit(data.test)
        data.nominal <- read.csv(paste("/data/uci/",fn,"/",fn,".nominal", sep=""), sep=",", header=F)
        data.nominal <- as.vector(as.matrix(data.nominal))
        
        
        ## Incert multiple Sample
        source("~/R/createData/InsertSample.R")
        #data.train <- insertNewValueToData(data.train, 100)
        #data.train <- insertNewSampleToData(data.train, 1000)
        
        ## PP Cluster
        source("~/R/ppdm/cluster.R")
        data.train <- ppCluster(data.train, 
                                merged.number=0, 
                                merged.rate = kMerged.rate, 
                                merged.size="max", 
                                rep.Method = "Merged")
        
        ## measure
        source("~/R/ppdm/get_PPDM_Measure.R")
        kano <- calKAnonymity(data.train)
        
        ## convert decisicon table class
        ## indx.nominalを自動的に決めれないか
        data.train   <- SF.asDecisionTable(data.train, decision.attr=ncol(data.train), indx.nominal=data.nominal)
        #true.classes <- data.test[,ncol(data.test), with=F][[1]]
        true.classes <- data.test[,ncol(data.test)]
        #data.test    <- SF.asDecisionTable(data.test[,-ncol(data.test), with=FALSE])
        data.test    <- SF.asDecisionTable(data.test[,-ncol(data.test)])
        
        ## 条件属性の離散化(と同時に、DecisionTable & data.frame型になってる)
        source("~/R/roughsets/My.Discretization.R")
        source("~/R/roughsets/My.ObjectFactory.R")
        data.train <- D.discretization.RST(data.train,
                                           type.method = "convert.nominal")
        
        ## cut value 不要
        #cut.values <- D.discretization.RST(data.train,
        #                                   type.method = "unsupervised.quantiles",
        #                                   nOfIntervals = 3)
        #data.train <- SF.applyDecTable(data.train, cut.values)
        #data.test  <- SF.applyDecTable(data.test,  cut.values)
        
        ## rule induction from the training set:
        source("~/R/roughsets/My.RI.LEM2Rules.RST.R")
        source("~/R/roughsets/My.RuleInduction.OtherFuncCollections.R")
        rules <- My.RI.MLEM2Rules.RST(data.train)
        #rules <- RI.LEM2Rules.RST(data.train)
        
        #rules
        #class(rules)
        #str(rules)
        
        ## predicitons for the test set:
        source("~/R/roughsets/LERS.R")
        pred.vals <- predict.LERS(rules, data.test)
        #pred.vals1 <- predict(rules, data.test, votingMethod = X.ruleStrength)
        #pred.vals2 <- predict(rules, data.test, votingMethod = X.laplace)
        #pred.vals3 <- predict(rules, data.test, votingMethod = X.rulesCounting)
        
        ## checking the accuracy of predictions:
        precision <- sprintf("%.3f", mean(pred.vals == true.classes))
        #precision <- c(sprintf("%.3f", mean(pred.vals1 == true.classes)), 
        #               sprintf("%.3f", mean(pred.vals2 == true.classes)),
        #               sprintf("%.3f", mean(pred.vals3 == true.classes)))      
        precision.sd <- sprintf("%.3f", sd(pred.vals == true.classes))
        source("~/R/ppdm/get_PPDM_Measure.R")
        entropy <- calEntropyForData(data.train)
        result <- paste(precision, "±", precision.sd, sep="")
        #result <- c(paste(sprintf("%.3f", mean(pred.vals1 == true.classes)),"±",sprintf("%.3f", sd(pred.vals1 == true.classes)), sep=""),
        #            paste(sprintf("%.3f", mean(pred.vals2 == true.classes)),"±",sprintf("%.3f", sd(pred.vals2 == true.classes)), sep=""),
        #            paste(sprintf("%.3f", mean(pred.vals3 == true.classes)),"±",sprintf("%.3f", sd(pred.vals3 == true.classes)), sep=""))
        
        ## append
        results.fn <- list.append(results.fn, 
                                  list(precision = precision, 
                                       precision_sd = precision.sd,
                                       entropy = entropy,
                                       kano = kano,
                                       result = result))
      }
    }
    for(i in kIter){
      names(results.fn) <- paste(fn,seq(1:kIter),sep="")
    }
    results <- list.append(results, results.fn)
  }
  names(results) <- kFilenames
  return(results)
}
