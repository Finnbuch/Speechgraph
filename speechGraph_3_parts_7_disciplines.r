setwd("E:/Wood/papers/Abstracts_graph_structure/same_size_5_year_all_sciences/same_size_5_year_all_sciences/")

allMyFiles         <- list.files(recursive = T)
findParameterTable <- grep("params_table.txt", allMyFiles, )

allParamTables <- allMyFiles[findParameterTable]


allFrames <- read.table(allParamTables[1], sep = ",", header = T)
allFrames$File2 <- allParamTables[1]

for(k in 1:length(allParamTables)){
  new       <- read.table(allParamTables[k], sep = ",", header = T)
  new$File2 <- allParamTables[k]
  allFrames <- rbind(allFrames,new)
}

############################# Creating the variable für aggregate ###################################
allFrames$hl   <- NA
allFrames$date <- NA 
allFrames$sci  <- substr(allFrames$File2, 1, 3)
allFrames$part <- substr(allFrames$File2, 5, 5)
allFrames$hl[substr(allFrames$File, 4, 4)=="h"] <- paste0("h", substr(allFrames$File[substr(allFrames$File, 4, 4)=="h"], 16, 20))
allFrames$hl[substr(allFrames$File, 4, 4)=="l"] <- paste0("l", substr(allFrames$File[substr(allFrames$File, 4, 4)=="l"], 15, 19))

allFrames$date[substr(allFrames$hl, 2, 3)=="05"] <- 2005
allFrames$date[substr(allFrames$hl, 2, 3)=="10"] <- 2010
allFrames$date[substr(allFrames$hl, 2, 3)=="80"] <- 1980
allFrames$date[substr(allFrames$hl, 2, 3)=="85"] <- 1985
allFrames$date[substr(allFrames$hl, 2, 3)=="90"] <- 1990
allFrames$date[substr(allFrames$hl, 2, 3)=="95"] <- 1995
allFrames$date[substr(allFrames$hl, 2, 3)=="00"] <- 2000
allFrames$date[substr(allFrames$hl, 2, 3)=="15"] <- 2015

################### normalized LSC RE plots ##################################################################################
par(mfrow = c(1, 1))
barplot(aggregate(allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)$x )      # Word count
RE  <- aggregate(allFrames$RE/allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean) 
LSC <- aggregate(allFrames$LSC/allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable

/allFrames$WC
par(mfrow = c(1, 3))
parts <- c("begin", "middle", "end")
par(mfrow = c(1, 3))
for(j in 1:3){
  lsc <- subset(LSC, substr(LSC$Group.2, 1, 1)=="l" & LSC$Group.4 == as.character(j))
  re  <- subset(RE , substr( RE$Group.2, 1, 1)=="l"& LSC$Group.4 == as.character(j))
  bio <- lsc[, 1] =="bio"
  che <- lsc[, 1] =="che"
  hum <- lsc[, 1] =="hum"
  inf <- lsc[, 1] =="inf"  
  mat <- lsc[, 1] =="mat" 
  phy <- lsc[, 1] =="phy"
  psy <- lsc[, 1] =="psy"  
  
  plot(lsc[bio, 5]~re[bio, 5], type = "b", col = 1, pch = 16, cex = 1.5, xlim = rev(c(0, 3)), ylim = c(10, 50), # xlim = rev(c(0, .06)), ylim = c(0.35, 0.7),
     xlab = "normalized RE", ylab = "normalized LSC", main = parts[j]) #  begin middle end
  points(lsc[che, 5]~re[che, 5], col = 2, pch = 17, cex = 1.5, type = "b")
  points(lsc[hum, 5]~re[hum, 5], col = 3, pch = 18, cex = 1.5, type = "b")
  points(lsc[inf, 5]~re[inf, 5], col = 4, pch = 19, cex = 1.5, type = "b")
  points(lsc[mat, 5]~re[mat, 5], col = 5, pch = 16, cex = 1.5, type = "b")
  points(lsc[phy, 5]~re[phy, 5], col = 6, pch = 17, cex = 1.5, type = "b")
  points(lsc[psy, 5]~re[psy, 5], col = 7, pch = 18, cex = 1.5, type = "b")
  points(lsc[lsc$Group.3 == "1980", 5]~re[re$Group.3 == "1980", 5], col = 1, pch = 9, cex = 2.5, type = "p")
  legend(x=0.055, y = 0.53, legend=c(levels(as.factor(lsc[, 1]))), col=c(1:7), cex=1.5, pch=c(16, 17, 18, 19, 16, 17, 18))
}



##################################################

RE  <- aggregate(allFrames$RE, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean) 
LSC <- aggregate(allFrames$LSC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable

summary(LSC)


parts <- c("begin", "middle", "end")
par(mfrow = c(1, 3))
for(j in 1:3){
  lsc <- subset(LSC, substr(LSC$Group.2, 1, 1)=="l" & LSC$Group.4 == as.character(j))
  re  <- subset(RE , substr( RE$Group.2, 1, 1)=="l"& LSC$Group.4 == as.character(j))
  bio <- lsc[, 1] =="bio"
  che <- lsc[, 1] =="che"
  hum <- lsc[, 1] =="hum"
  inf <- lsc[, 1] =="inf"  
  mat <- lsc[, 1] =="mat" 
  phy <- lsc[, 1] =="phy"
  psy <- lsc[, 1] =="psy"  
  
  plot(lsc[bio, 5]~re[bio, 5], type = "b", col = 1, pch = 16, cex = 1.5, xlim = rev(c(0.3, 4.3)), ylim = c(10, 38),
       xlab = "RE", ylab = "LSC", main = parts[j]) #    
  points(lsc[che, 5]~re[che, 5], col = 2, pch = 17, cex = 1.5, type = "b")
  points(lsc[hum, 5]~re[hum, 5], col = 3, pch = 18, cex = 1.5, type = "b")
  points(lsc[inf, 5]~re[inf, 5], col = 4, pch = 19, cex = 1.5, type = "b")
  points(lsc[mat, 5]~re[mat, 5], col = 5, pch = 16, cex = 1.5, type = "b")
  points(lsc[phy, 5]~re[phy, 5], col = 6, pch = 17, cex = 1.5, type = "b")
  points(lsc[psy, 5]~re[psy, 5], col = 7, pch = 18, cex = 1.5, type = "b")
  
  legend(x=4, y = 20, legend=c(levels(as.factor(lsc[, 1]))), col=c(1:7), cex=1.5, pch=c(16, 17, 18, 19, 16, 17, 18))
}



#################################### Other Speechgraph values ########################


aggregate(allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)
N <- aggregate(allFrames$Nodes, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)    # Nr Nodes
aggregate(allFrames$Edges, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)    # Nr Edges
aggregate(allFrames$RE, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)       # Repeated edges, sum of all edges linking the same pair of nodes
aggregate(allFrames$PE/allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)       # Parallel Edges (PE) = Total number of edges linking the same pair of nodes more than once
aggregate(allFrames$L1, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)      # Loops of length 1
L2 <- aggregate(allFrames$L2, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)       # Loops of length 2
L3 <- aggregate(allFrames$L3, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)       # Loops of length 3
aggregate(allFrames$LCC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)      # Largest connected component
aggregate(allFrames$LSC/allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable
ATD <- aggregate(allFrames$ATD/allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)      # Average Total Degree (ATD)
aggregate(allFrames$Density/allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)  #
aggregate(allFrames$Diameter/allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean) #
aggregate(allFrames$ASP/allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)      # Average shortest path
aggregate(allFrames$CC, by= list(allFrames$sci, allFrames$hl, allFrames$date, allFrames$part), FUN = mean)

summary(N)
summary(ATD) 
parts <- c("begin", "middle", "end")
par(mfrow = c(1, 3))
for(j in 1:3){
  lsc <- subset(ATD, substr(LSC$Group.2, 1, 1)=="l" & LSC$Group.4 == as.character(j))
  re  <- subset(L3 , substr( RE$Group.2, 1, 1)=="l"& LSC$Group.4 == as.character(j))
  bio <- lsc[, 1] =="bio"
  che <- lsc[, 1] =="che"
  hum <- lsc[, 1] =="hum"
  inf <- lsc[, 1] =="inf"  
  mat <- lsc[, 1] =="mat" 
  phy <- lsc[, 1] =="phy"
  psy <- lsc[, 1] =="psy"  
  
  plot(lsc[bio, 5]~re[bio, 5], type = "b", col = 1, pch = 16, cex = 1.5, xlim = rev(c(0, 3)), ylim = c(0.05, 0.12),
       xlab = "Loops of length 3", ylab = "normalized ATD", main = parts[j]) #    
  points(lsc[che, 5]~re[che, 5], col = 2, pch = 17, cex = 1.5, type = "b")
  points(lsc[hum, 5]~re[hum, 5], col = 3, pch = 18, cex = 1.5, type = "b")
  points(lsc[inf, 5]~re[inf, 5], col = 4, pch = 19, cex = 1.5, type = "b")
  points(lsc[mat, 5]~re[mat, 5], col = 5, pch = 16, cex = 1.5, type = "b")
  points(lsc[phy, 5]~re[phy, 5], col = 6, pch = 17, cex = 1.5, type = "b")
  points(lsc[psy, 5]~re[psy, 5], col = 7, pch = 18, cex = 1.5, type = "b")
  
  legend(x=2.8, y = 0.12, legend=c(levels(as.factor(lsc[, 1]))), col=c(1:7), cex=1.5, pch=c(16, 17, 18, 19, 16, 17, 18))
}




summary(aov(WC       ~ time*affe, data = AllData))
summary(aov(Nodes    ~ time*affe, data = AllData))
summary(aov(Edges    ~ time*affe, data = AllData))
summary(aov(RE       ~ time*affe, data = AllData))
summary(aov(PE/WC       ~ time*affe, data = AllData))
summary(aov(L1       ~ time*affe, data = AllData))
summary(aov(L2       ~ time*affe, data = AllData))
summary(aov(L3       ~ time*affe, data = AllData))
summary(aov(LCC      ~ time*affe, data = AllData))
summary(aov(LSC/AllData$WC      ~ time*affe, data = AllData))
summary(aov(ATD/WC      ~ time*affe, data = AllData))
summary(aov(Density/WC  ~ time*affe, data = AllData))
summary(aov(Diameter/WC ~ time*affe, data = AllData))
summary(aov(ASP/WC      ~ time*affe, data = AllData))
summary(aov(CC       ~ time*affe, data = AllData))






















install.packages("GGally")
library(GGally)

ggpairs(AllData[, 2:16])

prop.test(c(38, 26), c(64, 64) )

stderror <- function(x) sd(x)/sqrt(length(x))
par(mfrow = c(1, 2))

  setwd(paths[i]) # sets the correct path to the three disciplines
  
allFiles <- list.files()
indParam <- grep("params_table.txt", allFiles)
AllData <-  read.table(allFiles[indParam], sep = ",", header = T)
colnames(AllData)
AllData$affe[grep("low", AllData$File)] <- as.factor(substr(AllData$File, 1, 3))[grep("low", AllData$File)]
AllData$affe[grep("low", AllData$File, invert = T)] <- as.factor(substr(AllData$File, 1, 4))[grep("low", AllData$File, invert = T)]

AllData$time[grep("low", AllData$File)] <- substr(AllData$File, 12, 13)[grep("low", AllData$File)]
AllData$time[grep("low", AllData$File, invert = T)] <- substr(AllData$File, 13, 14)[grep("low", AllData$File, invert = T)]

AllData$File[grep("low", AllData$File)] <- substr(AllData$File, 1, 13)[grep("low", AllData$File)]
AllData$File[grep("low", AllData$File, invert = T)] <- substr(AllData$File, 1, 14)[grep("low", AllData$File, invert = T)]

#table(AllData$File)

aggregate(AllData$WC, by= list(AllData$File), FUN = mean)       # Word count
aggregate(AllData$WC, by= list(AllData$File), FUN = quantile, probs = c(0.025, 0.975))  
summary(aov(WC       ~ time*affe, data = AllData))

#re   <- aggregate(AllData$RE, by= list(AllData$File), FUN = mean)       # Repeated edges, sum of all edges linking the same pair of nodes
#recp <- aggregate(AllData$RE, by= list(AllData$File), FUN = stderror)
re    <- aggregate(AllData$RE/AllData$WC, by= list(AllData$File), FUN = mean)       # Repeated edges, sum of all edges linking the same pair of nodes
recp  <- aggregate(AllData$RE/AllData$WC, by= list(AllData$File), FUN = stderror)
lsci <- reci  <- matrix(NA, ncol = 2, nrow = length(table(AllData$File)))
reci[1:length(table(AllData$File)), 2] <- re[, 2] - recp[, 2]
reci[1:length(table(AllData$File)), 1] <- re[, 2] + recp[, 2]
summary(aov(RE      ~ time*affe, data = AllData))   # i

#lsc  <- aggregate(AllData$LSC, by= list(AllData$File), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable
#lscp <- aggregate(AllData$LSC, by= list(AllData$File), FUN = stderror)
lsc  <- aggregate(AllData$LSC/AllData$WC, by= list(AllData$File), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable
lscp <- aggregate(AllData$LSC/AllData$WC, by= list(AllData$File), FUN = stderror)
lsci[1:length(table(AllData$File)), 2] <- lsc[, 2] - lscp[, 2]
lsci[1:length(table(AllData$File)), 1] <- lsc[, 2] + lscp[, 2]
summary(aov(LSC      ~ time*affe, data = AllData))

high <- c(1:5)
low <- c(6:10)
all <- c(1:10)

#if(i == 1){plot(lsc[1:5, 2]~re[1:5, 2], col = 1, pch = 16, cex = 1.5, xlim = rev(c(3, 13)), ylim = c(40, 100), xlab = "Repeated edges",ylab = "Largest strongly connected cluster")
if(i == 1){plot(lsc[low, 2]~re[low, 2], col = 1, pch = 16, cex = 1.5, xlim = rev(c(0.00, .12)), ylim = c(0.55, 0.8), xlab = " normalized repeated edges",ylab = "Normalized largest strongly connected cluster")
  text(  lsc[low, 2]~ re[low, 2], labels = c("2000", "2010", "2020", "1980", "1990"), pos = 1, offset = 0.5, cex = 0.5)
  #arrows(reci[1, 2],  lsc[1, 2], reci[1, 1], length = 0)
  #arrows(re[1, 2],    lsci[1, 2], re[1, 2], lsci[1, 1], length = 0)
  #arrows(reci[2, 2],  lsc[2, 2], reci[2, 1], length = 0)
  #arrows(re[2, 2],    lsci[2, 2], re[2, 2], lsci[2, 1], length = 0)
  #arrows(reci[3, 2],  lsc[3, 2], reci[3, 1], length = 0)
  #arrows(re[3, 2],    lsci[3, 2], re[3, 2], lsci[3, 1], length = 0)
}else
{points(lsc[low, 2]~ re[low, 2], col = i, pch = 16, cex = 1.5)
  text( lsc[low, 2]~ re[low, 2], labels = c("2000", "2010", "2020", "1980", "1990"), pos = 1, offset = 0.5, cex = 0.5)
  #arrows(reci[1, 2], lsc[1, 2], reci[1, 1], length = 0, col = i)
  #arrows(re[1, 2],   lsci[1, 2], re[1, 2], lsci[1, 1], length = 0, col = i)
  #arrows(reci[2, 2], lsc[2, 2], reci[2, 1], length = 0, col = i)
  #arrows(re[2, 2],   lsci[2, 2], re[2, 2], lsci[2, 1], length = 0, col = i)
  #arrows(reci[3, 2], lsc[3, 2], reci[3, 1], length = 0, col = i)
  #arrows(re[3, 2],   lsci[3, 2], re[3, 2], lsci[3, 1], length = 0, col = i)
  }

  legend(x=0.12, y = 0.75, legend=c("bio", "phys", "psy"), col=c(1:3), cex=1, lty=1)
  
}

####### Analysis of lexical diversity, short range recurrence, long range recurrence & graph length #######

paths <- c("E:/Wood/papers/Abstracts_graph_structure/txt_decades/txt_decades/bio",
           "E:/Wood/papers/Abstracts_graph_structure/txt_decades/txt_decades/phy",
           "E:/Wood/papers/Abstracts_graph_structure/txt_decades/txt_decades/psy")

stderror <- function(x) sd(x)/sqrt(length(x))

par(mfrow = c(1, 3))

for(i in 1:length(paths)){
  
  setwd(paths[i]) # sets the correct path to the three disciplines
  
  allFiles <- list.files()
  indParam <- grep("params_table.txt", allFiles)
  AllData <-  read.table(allFiles[indParam], sep = ",", header = T)
  colnames(AllData)
  AllData$affe[grep("low", AllData$File)] <- as.factor(substr(AllData$File, 1, 3))[grep("low", AllData$File)]
  AllData$affe[grep("low", AllData$File, invert = T)] <- as.factor(substr(AllData$File, 1, 4))[grep("low", AllData$File, invert = T)]
  
  AllData$time[grep("low", AllData$File)] <- substr(AllData$File, 12, 13)[grep("low", AllData$File)]
  AllData$time[grep("low", AllData$File, invert = T)] <- substr(AllData$File, 13, 14)[grep("low", AllData$File, invert = T)]
  
  AllData$File[grep("low", AllData$File)] <- substr(AllData$File, 1, 13)[grep("low", AllData$File)]
  AllData$File[grep("low", AllData$File, invert = T)] <- substr(AllData$File, 1, 14)[grep("low", AllData$File, invert = T)]
  
  aggregate(AllData$WC, by= list(AllData$File), FUN = mean)       # Word count
  aggregate(AllData$WC, by= list(AllData$File), FUN = quantile, probs = c(0.025, 0.975))  
  summary(aov(WC       ~ time*affe, data = AllData))
  
  summary(aov(Nodes    ~ time*affe, data = AllData))
  
  recp <- aggregate(AllData$RE, by= list(AllData$File), FUN = stderror)
  #re   <- aggregate(AllData$RE/AllData$WC, by= list(AllData$File), FUN = mean)       # Repeated edges, sum of all edges linking the same pair of nodes
  #recp <- aggregate(AllData$RE/AllData$WC, by= list(AllData$File), FUN = stderror)
  reci[, 2] <- re[, 2] - recp[, 2]
  reci[, 1] <- re[, 2] + recp[, 2]
  summary(aov(RE       ~ time*affe, data = AllData))
  
  aggregate(AllData$PE, by= list(AllData$File), FUN = mean)       # Parallel Edges (PE) = Total number of edges linking the same pair of nodes more than once
  summary(aov(PE       ~ time*affe, data = AllData))
  aggregate(AllData$L1, by= list(AllData$File), FUN = mean)      # Loops of length 1
  summary(aov(L1       ~ time*affe, data = AllData))
  aggregate(AllData$L2, by= list(AllData$File), FUN = mean)       # Loops of length 2
  summary(aov(L2       ~ time*affe, data = AllData))
  aggregate(AllData$L3, by= list(AllData$File), FUN = mean)       # Loops of length 3
  summary(aov(L3       ~ time*affe, data = AllData))
  aggregate(AllData$LCC, by= list(AllData$File), FUN = mean)      # Largest connected component
  summary(aov(LCC      ~ time*affe, data = AllData))
  
  lscp <- aggregate(AllData$LSC, by= list(AllData$File), FUN = stderror)
  #lsc  <- aggregate(AllData$LSC/AllData$WC, by= list(AllData$File), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable
  #lscp <- aggregate(AllData$LSC/AllData$WC, by= list(AllData$File), FUN = stderror)
  lsci[, 2] <- lsc[, 2] - lscp[, 2]
  lsci[, 1] <- lsc[, 2] + lscp[, 2]
  summary(aov(LSC      ~ time*affe, data = AllData))
  
  aggregate(AllData$ATD, by= list(AllData$File), FUN = mean)      # Average Total Degree (ATD)
  summary(aov(ATD/WC      ~ time*affe, data = AllData))
  aggregate(AllData$Density/AllData$WC, by= list(AllData$File), FUN = mean)  #
  summary(aov(Density/WC  ~ time*affe, data = AllData))
  aggregate(AllData$Diameter/AllData$WC, by= list(AllData$File), FUN = mean) #
  summary(aov(Diameter/WC ~ time*affe, data = AllData))
  
  n   <- aggregate(AllData$Nodes/AllData$WC, by= list(AllData$File), FUN = mean)    # Nr Nodes
  re  <- aggregate(AllData$RE/AllData$WC, by= list(AllData$File), FUN = mean)       # Repeated edges, sum of all edges linking the same pair of nodes
  lsc <- aggregate(AllData$LSC/AllData$WC, by= list(AllData$File), FUN = mean)# Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable
  asp <- aggregate(AllData$ASP/AllData$WC, by= list(AllData$File), FUN = mean)      # Average shortest path
  summary(aov(ASP/WC      ~ time*affe, data = AllData))
 
  #   if(i == 1){
  #     barplot(n[c(3, 6, 1, 4, 2, 5), 2], col = c(1, 1, 2, 2, 3, 3), names.arg = c("h_80", "l_80","h_00", "l_00", "h_20", "l_20"), main = "N")
  #     barplot(re[c(3, 6, 1, 4, 2, 5), 2], col = c(1, 1, 2, 2, 3, 3), names.arg = c("h_80", "l_80","h_00", "l_00", "h_20", "l_20"), main = "RE")
  #     barplot(lsc[c(3, 6, 1, 4, 2, 5), 2], col = c(1, 1, 2, 2, 3, 3), names.arg = c("h_80", "l_80","h_00", "l_00", "h_20", "l_20"), main = "LSC")
  #     barplot(asp[c(3, 6, 1, 4, 2, 5), 2], col = c(1, 1, 2, 2, 3, 3), names.arg = c("h_80", "l_80","h_00", "l_00", "h_20", "l_20"), main = "ASP")
  #     
  # }else
  # { barplot(n[c(3, 6, 1, 4, 2, 5), 2], col = c(1, 1, 2, 2, 3, 3), names.arg = c("h_80", "l_80","h_00", "l_00", "h_20", "l_20"), main = "N")
  #   barplot(re[c(3, 6, 1, 4, 2, 5), 2], col = c(1, 1, 2, 2, 3, 3), names.arg = c("h_80", "l_80","h_00", "l_00", "h_20", "l_20"), main = "RE")
  #   barplot(lsc[c(3, 6, 1, 4, 2, 5), 2], col = c(1, 1, 2, 2, 3, 3), names.arg = c("h_80", "l_80","h_00", "l_00", "h_20", "l_20"), main = "LSC")
  #   barplot(asp[c(3, 6, 1, 4, 2, 5), 2], col = c(1, 1, 2, 2, 3, 3), names.arg = c("h_80", "l_80","h_00", "l_00", "h_20", "l_20"), main = "ASP")
  #  } 
  
  # To coment large passages, Ctrl+Shift+C in RStudio 
  
}

################################## Plots of low vs high emotion ##############################################

paths <- c("E:/Wood/papers/Abstracts_graph_structure/txt_decades/txt_decades/bio",
           "E:/Wood/papers/Abstracts_graph_structure/txt_decades/txt_decades/phy",
           "E:/Wood/papers/Abstracts_graph_structure/txt_decades/txt_decades/psy")

#paths <- c("E:/Wood/papers/ClosedFiles/Edlinger_science/txt_samples/txt_bio", 
#           "E:/Wood/papers/ClosedFiles/Edlinger_science/txt_samples/txt_phy",
#           "E:/Wood/papers/ClosedFiles/Edlinger_science/txt_samples/txt_psy")

stderror <- function(x) sd(x)/sqrt(length(x))
par(mfrow = c(1, 3))

rm(AllData, asp, lsc, lsci, lscp, n, re, reci, recp)

disciplines <- c("biology", "physics", "psychology")

for(i in 1:length(paths)){
  
  setwd(paths[i]) # sets the correct path to the three disciplines
  
  allFiles <- list.files()
  indParam <- grep("params_table.txt", allFiles)
  AllData <-  read.table(allFiles[indParam], sep = ",", header = T)
  colnames(AllData)
  AllData$affe[grep("low", AllData$File)] <- as.factor(substr(AllData$File, 1, 3))[grep("low", AllData$File)]
  AllData$affe[grep("low", AllData$File, invert = T)] <- as.factor(substr(AllData$File, 1, 4))[grep("low", AllData$File, invert = T)]
  
  AllData$time[grep("low", AllData$File)] <- substr(AllData$File, 12, 13)[grep("low", AllData$File)]
  AllData$time[grep("low", AllData$File, invert = T)] <- substr(AllData$File, 13, 14)[grep("low", AllData$File, invert = T)]
  
  AllData$File[grep("low", AllData$File)] <- substr(AllData$File, 1, 13)[grep("low", AllData$File)]
  AllData$File[grep("low", AllData$File, invert = T)] <- substr(AllData$File, 1, 14)[grep("low", AllData$File, invert = T)]
  
  #table(AllData$File)
  
  aggregate(AllData$WC, by= list(AllData$File), FUN = mean)       # Word count
  aggregate(AllData$WC, by= list(AllData$File), FUN = quantile, probs = c(0.025, 0.975))  
  summary(aov(WC       ~ time*affe, data = AllData))
  
  #re   <- aggregate(AllData$RE, by= list(AllData$File), FUN = mean)       # Repeated edges, sum of all edges linking the same pair of nodes
  #recp <- aggregate(AllData$RE, by= list(AllData$File), FUN = stderror)
  re    <- aggregate(AllData$RE/AllData$WC, by= list(AllData$File), FUN = mean)       # Repeated edges, sum of all edges linking the same pair of nodes
  recp  <- aggregate(AllData$RE/AllData$WC, by= list(AllData$File), FUN = stderror)
  lsci <- reci  <- matrix(NA, ncol = 2, nrow = length(table(AllData$File)))
  reci[1:length(table(AllData$File)), 2] <- re[, 2] - recp[, 2]
  reci[1:length(table(AllData$File)), 1] <- re[, 2] + recp[, 2]
  summary(aov(RE      ~ time*affe, data = AllData))   # i
  
  #lsc  <- aggregate(AllData$LSC, by= list(AllData$File), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable
  #lscp <- aggregate(AllData$LSC, by= list(AllData$File), FUN = stderror)
  lsc  <- aggregate(AllData$LSC/AllData$WC, by= list(AllData$File), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable
  lscp <- aggregate(AllData$LSC/AllData$WC, by= list(AllData$File), FUN = stderror)
  lsci[1:length(table(AllData$File)), 2] <- lsc[, 2] - lscp[, 2]
  lsci[1:length(table(AllData$File)), 1] <- lsc[, 2] + lscp[, 2]
  summary(aov(LSC      ~ time*affe, data = AllData))
  
  high <- c(1:5)
  low <- c(6:10)
  all <- c(1:10)
  
  #xlim = rev(c(3, 13)), ylim = c(40, 100)
  #xlim = rev(c(0.03, .08)), ylim = c(0.65, 0.72)
  plot(lsc[high, 2]~re[high, 2], col = 1, pch = 16, cex = 1.5, xlim = rev(c(0.03, .08)), ylim = c(0.65, 0.72), xlab = " normalized repeated edges",ylab = "Normalized largest strongly connected cluster", main = disciplines[i])
    text(  lsc[high, 2]~ re[high, 2], labels = c("2000", "2010", "2020", "1980", "1990"), pos = 1, offset = 0.5, cex = 0.5)

  points(lsc[low, 2]~ re[low, 2], col = 2, pch = 16, cex = 1.5)
    text( lsc[low, 2]~ re[low, 2], labels = c("2000", "2010", "2020", "1980", "1990"), pos = 1, offset = 0.5, cex = 0.5)
    

  legend(x=0.07, y = 0.72, legend=c("high Emotion", "low emotion"), col=c(1:3), cex=1, lty=1)
  
}



######################################## Analysis of the thirds ###############################################

paths <- c("E:/Wood/papers/Abstracts_graph_structure/txt_thirds/txt_thirds/bio/1",
           "E:/Wood/papers/Abstracts_graph_structure/txt_thirds/txt_thirds/phy/1",
           "E:/Wood/papers/Abstracts_graph_structure/txt_thirds/txt_thirds/psy/1")

paths <- c("E:/Wood/papers/Abstracts_graph_structure/txt_thirds/txt_thirds/bio/2",
           "E:/Wood/papers/Abstracts_graph_structure/txt_thirds/txt_thirds/phy/2",
           "E:/Wood/papers/Abstracts_graph_structure/txt_thirds/txt_thirds/psy/2")

paths <- c("E:/Wood/papers/Abstracts_graph_structure/txt_thirds/txt_thirds/bio/3",
           "E:/Wood/papers/Abstracts_graph_structure/txt_thirds/txt_thirds/phy/3",
           "E:/Wood/papers/Abstracts_graph_structure/txt_thirds/txt_thirds/psy/3")


stderror <- function(x) sd(x)/sqrt(length(x))
par(mfrow = c(1, 3))

rm(AllData, asp, lsc, lsci, lscp, n, re, reci, recp)

disciplines <- c("biology", "physics", "psychology")

myColor <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), ncol = 3, nrow = 3, byrow = T)
satur   <- c(0.2, 0.5, 1)

for(i in 1:length(paths)){
  
  setwd(paths[i]) # sets the correct path to the three disciplines
  
  allFiles <- list.files()
  indParam <- grep("params_table.txt", allFiles)
  AllData <-  read.table(allFiles[indParam], sep = ",", header = T)
  colnames(AllData)
  AllData$affe[grep("low", AllData$File)] <- as.factor(substr(AllData$File, 1, 3))[grep("low", AllData$File)]
  AllData$affe[grep("low", AllData$File, invert = T)] <- as.factor(substr(AllData$File, 1, 4))[grep("low", AllData$File, invert = T)]
  
  AllData$time[grep("low", AllData$File)] <- substr(AllData$File, 12, 13)[grep("low", AllData$File)]
  AllData$time[grep("low", AllData$File, invert = T)] <- substr(AllData$File, 13, 14)[grep("low", AllData$File, invert = T)]
  
  AllData$File[grep("low", AllData$File)] <- substr(AllData$File, 1, 13)[grep("low", AllData$File)]
  AllData$File[grep("low", AllData$File, invert = T)] <- substr(AllData$File, 1, 14)[grep("low", AllData$File, invert = T)]
  
  aggregate(AllData$WC, by= list(AllData$File), FUN = mean)       # Word count
  aggregate(AllData$WC, by= list(AllData$File), FUN = quantile, probs = c(0.025, 0.975))  
  summary(aov(WC       ~ time*affe, data = AllData))
  
  #re   <- aggregate(AllData$RE, by= list(AllData$File), FUN = mean)       # Repeated edges, sum of all edges linking the same pair of nodes
  #recp <- aggregate(AllData$RE, by= list(AllData$File), FUN = stderror)
  re    <- aggregate(AllData$RE/AllData$WC, by= list(AllData$File), FUN = mean)       # Repeated edges, sum of all edges linking the same pair of nodes
  recp  <- aggregate(AllData$RE/AllData$WC, by= list(AllData$File), FUN = stderror)
  lsci <- reci  <- matrix(NA, ncol = 2, nrow = length(table(AllData$File)))
  reci[1:length(table(AllData$File)), 2] <- re[, 2] - recp[, 2]
  reci[1:length(table(AllData$File)), 1] <- re[, 2] + recp[, 2]
  summary(aov(RE      ~ time*affe, data = AllData))   # i
  
  #lsc  <- aggregate(AllData$LSC, by= list(AllData$File), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable
  #lscp <- aggregate(AllData$LSC, by= list(AllData$File), FUN = stderror)
  lsc  <- aggregate(AllData$LSC/AllData$WC, by= list(AllData$File), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable
  lscp <- aggregate(AllData$LSC/AllData$WC, by= list(AllData$File), FUN = stderror)
  lsci[1:length(table(AllData$File)), 2] <- lsc[, 2] - lscp[, 2]
  lsci[1:length(table(AllData$File)), 1] <- lsc[, 2] + lscp[, 2]
  summary(aov(LSC      ~ time*affe, data = AllData))
  
  high <- c(1:3)
  low <- c(4:6)
  all <- c(1:6)
  
  myCol <- rgb(myColor[i, 1], myColor[i, 2], myColor[i, 3], alpha = satur[i]) 
  
  #if(i == 1){plot(lsc[1:5, 2]~re[1:5, 2], col = 1, pch = 16, cex = 1.5, xlim = rev(c(3, 13)), ylim = c(40, 100), xlab = "Repeated edges",ylab = "Largest strongly connected cluster")
  if(i == 1){plot(lsc[low, 2]~re[low, 2], col = 1, pch = 16, cex = 1.5, xlim = rev(c(0.005, .035)), ylim = c(0.35, 0.62), xlab = " normalized repeated edges",ylab = "Normalized largest strongly connected cluster")
    text(  lsc[low, 2]~ re[low, 2], labels = c("2000", "2020", "1980"), pos = 1, offset = 0.5, cex = 1)
    }else
  {points(lsc[low, 2]~ re[low, 2], col = myCol, pch = 16, cex = 1.5)
    text( lsc[low, 2]~ re[low, 2], labels = c("2000", "2020", "1980"), pos = 1, offset = 0.5, cex = 1)
    }
  
  legend(x=0.035, y = 0.4, legend=c("bio", "phys", "psy"), col=c(1:3), cex=1, lty=1)
  
}

########################################## Analysis of whole text ####################################

setwd("E:/Wood/papers/Abstracts_graph_structure/same_size_5_year_all_sciences_whole_texts/same_size_5_year_all_sciences_whole_texts/")

allMyFiles         <- list.files(recursive = T)
findParameterTable <- grep("params_table.txt", allMyFiles, )

allParamTables <- allMyFiles[findParameterTable]


allFrames <- read.table(allParamTables[1], sep = ",", header = T)
allFrames$File2 <- allParamTables[1]

for(k in 1:length(allParamTables)){
  new       <- read.table(allParamTables[k], sep = ",", header = T)
  new$File2 <- allParamTables[k]
  allFrames <- rbind(allFrames,new)
}

############################# Creating the variable für aggregate ###################################
allFrames$hl   <- NA
allFrames$date <- NA 
allFrames$sci  <- substr(allFrames$File2, 1, 3)
allFrames$part <- substr(allFrames$File2, 5, 5)
allFrames$hl[substr(allFrames$File, 4, 4)=="h"] <- paste0("h", substr(allFrames$File[substr(allFrames$File, 4, 4)=="h"], 16, 20))
allFrames$hl[substr(allFrames$File, 4, 4)=="l"] <- paste0("l", substr(allFrames$File[substr(allFrames$File, 4, 4)=="l"], 15, 19))

allFrames$date[substr(allFrames$hl, 2, 3)=="05"] <- 2005
allFrames$date[substr(allFrames$hl, 2, 3)=="10"] <- 2010
allFrames$date[substr(allFrames$hl, 2, 3)=="80"] <- 1980
allFrames$date[substr(allFrames$hl, 2, 3)=="85"] <- 1985
allFrames$date[substr(allFrames$hl, 2, 3)=="90"] <- 1990
allFrames$date[substr(allFrames$hl, 2, 3)=="95"] <- 1995
allFrames$date[substr(allFrames$hl, 2, 3)=="00"] <- 2000
allFrames$date[substr(allFrames$hl, 2, 3)=="15"] <- 2015

################### normalized LSC RE plots ##################################################################################
par(mfrow = c(1, 1))
#barplot(aggregate(allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date), FUN = mean)$x )      # Word count
RE  <- aggregate(allFrames$RE/allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date), FUN = mean) 
LSC <- aggregate(allFrames$LSC/allFrames$WC, by= list(allFrames$sci, allFrames$hl, allFrames$date), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable
ATD  <- aggregate(allFrames$ATD, by= list(allFrames$sci, allFrames$hl, allFrames$date), FUN = mean) 
L3 <- aggregate(allFrames$L3, by= list(allFrames$sci, allFrames$hl, allFrames$date), FUN = mean)      # Largest Strongly Connected Component (LSC) = Total number of nodes comprising the largest sub-graph in which all nodes are mutually reachable

par(mfrow = c(1, 1))

for(j in 1:1){
  lsc <- subset(LSC, substr(LSC$Group.2, 1, 1)=="h" )
  re  <- subset(RE , substr( RE$Group.2, 1, 1)=="h")
  bio <- lsc[, 1] =="bio"
  che <- lsc[, 1] =="che"
  hum <- lsc[, 1] =="hum"
  inf <- lsc[, 1] =="inf"  
  mat <- lsc[, 1] =="mat" 
  phy <- lsc[, 1] =="phy"
  psy <- lsc[, 1] =="psy"  
  
  plot(lsc[bio, 4]~re[bio, 4], type = "b", col = 1, pch = 16, cex = 1.5, xlim = rev(c(0.02, 0.1)), ylim = c(0.6, 0.74), xlab = "normalized RE", ylab = "normalized LSC", main = parts[j]) #  begin middle end
  #plot(lsc[bio, 4]~re[bio, 4], type = "b", col = 1, pch = 16, cex = 1.5, xlim = rev(c(0.02, 2)), ylim = c(0.6, 0.74), xlab = "normalized RE", ylab = "normalized LSC", main = parts[j])
  points(lsc[lsc$Group.3 == "1980", 4]~re[re$Group.3 == "1980", 4], col = 1, pch = 9, cex = 2.5, type = "p")
  points(lsc[che, 4]~re[che, 4], col = 2, pch = 17, cex = 1.5, type = "b")
  points(lsc[hum, 4]~re[hum, 4], col = 3, pch = 18, cex = 1.5, type = "b")
  points(lsc[inf, 4]~re[inf, 4], col = 4, pch = 19, cex = 1.5, type = "b")
  points(lsc[mat, 4]~re[mat, 4], col = 5, pch = 16, cex = 1.5, type = "b")
  points(lsc[phy, 4]~re[phy, 4], col = 6, pch = 17, cex = 1.5, type = "b")
  points(lsc[psy, 4]~re[psy, 4], col = 7, pch = 18, cex = 1.5, type = "b")
 
  legend(x=0.1, y = 0.74, legend=c(levels(as.factor(lsc[, 1]))), col=c(1:7), cex=1.5, pch=c(16, 17, 18, 19, 16, 17, 18))
}


xtabs(~cbind(lsc, re[, 4])$Group.1 + cbind(lsc, re[, 4])$Group.2)

out <- cbind(LSC, y=RE[, 4])
out$z <-out$x/out$y
out$Group.2 <- substr(out$Group.2, 1, 1)


require(lattice)

col.out <- rainbow(8)[(gl(8, 14))]
key.out <- list(space = "right", text = list(c("high emotion", "low emotion"), points = list(pch = 1:2, col = 1:2)))
plot1 <- xyplot(x~y| Group.1, groups = Group.2, data = out, aspect = 2, layout = c(7, 1), cex = 1, pch = 16, fill = col.out,
       type = c("o", "g"), xlab = "normalized RE", ylab = "normalized LSC", auto.key = list(space = "right")) # aspect = "xy",


plot2 <- xyplot(z~Group.3| Group.1, groups = Group.2, data = out, aspect = 2, layout = c(7, 1), cex = 1, pch = 16, fill = col.out,
       type = c("o", "g"), xlab = "year of publication", ylab = "normalized LSC/normalized RE", auto.key = list(space = "right"))


#install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot1, plot2, ncol = 1, nrow = 2, heights = c(3, 3), widths = c(3))

############################# Correlations of graph measures #################################################
require(corrplot)

allFrames$sci  <- substr(allFrames$File2, 1, 3)
allFrames$part <- substr(allFrames$File2, 5, 5)
allFrames$hl[substr(allFrames$File, 4, 4)=="h"] <- paste0("h", substr(allFrames$File[substr(allFrames$File, 4, 4)=="h"], 16, 20))
allFrames$hl[substr(allFrames$File, 4, 4)=="l"] <- paste0("l", substr(allFrames$File[substr(allFrames$File, 4, 4)=="l"], 15, 19))

allFrames$date

colnames(allFrames)
head(allFrames)
#allFrames$date == 2015 && 
  
graphP <- subset(allFrames[, c(2:16)], substr(allFrames$hl, 1, 1) == "l")
corrplot(cor(graphP))
