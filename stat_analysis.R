### statistical analysis and plotting of speechgraph data ###

df <- read.csv("E:\\Speechgraphs_paper\\same_size_5_year_all_sciences_thirds\\final_table_non_retracted.csv")
df_retracted <- read.csv("E:\\Speechgraphs_paper\\rejected_abstracts\\same_size_5_year_all_sciences_thirds\\final_table_retracted")


df$Science <- factor(df$Science, levels = c("bio", "psy", "phy", "hum", "mat", "inf", "che" ))


#myInd <- sample(1:length(full_dataset$pos_1_binary), 10000)
nForBoot    <- 10000
repeats     <- 1000








myIndMatrix <- as.matrix(mapply(function(k) sample(1:length(df$Valence), nForBoot), 1:repeats))
fit.1 <- matrix(NA, ncol = 21, nrow=repeats)
odds_ratios <- matrix(NA, ncol = 7, nrow=repeats)
LRT <- matrix(NA, ncol = 1, nrow=repeats)
PseudR2 <- matrix(NA, ncol = 1, nrow=repeats)


for(i in 1:repeats){
  myInd     <- myIndMatrix[, i]
  my_y      <- c(as.factor(full_dataset$pos_1_binary[myInd]), as.factor(full_dataset$pos_2_binary[myInd]), as.factor(full_dataset$pos_3_binary[myInd]))
  my_length <- rep(full_dataset$length_rescaled[myInd], 3)
  my_date   <- rep(full_dataset$Date_rescaled[myInd], 3)
  my_scie   <- rep(full_dataset$Science[myInd], 3)
  my_part   <- gl(3, length(my_length)/3)
  
  
  fit.1[i , ] <- summary(glm(my_y ~ my_length + my_date + my_scie + my_part, family=binomial(link = "logit")))$coefficients[c(1:7, 15:28)]
  fit.2 <- glm(my_y ~ my_length + my_date + my_scie + my_part, family=binomial(link = "logit"))
  PseudR2[i,] <- PseudoR2(fit.2, which = "Nagelkerke")
  odds_ratios[i,] <- exp(coef(fit.2))
}
