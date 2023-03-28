####################################
### Descriptive data
### for "Does educational attainment reduce religiosity?"
### by Benjamin Grant Purzycki and Theiss Bendixen
###########################################

data <- read.delim("ALLSITES_V3.7_tabdel.txt")

d <- data[data$SITE != "Hadza", ] 

aggregate(d$SEX, list(d$SITE), FUN = sum, na.rm = T)
aggregate(d$AGE, list(d$SITE), FUN = mean, na.rm = T) 
aggregate(d$AGE, list(d$SITE), FUN = sd, na.rm = T) 
aggregate(d$FORMALED, list(d$SITE), FUN = mean, na.rm = T) 
aggregate(d$FORMALED, list(d$SITE), FUN = sd, na.rm = T) 

length(unique(d$CID))
sum(d$SEX, na.rm = T)
mean(d$AGE, na.rm = T)
sd(d$AGE, na.rm = T)
mean(d$FORMALED, na.rm = T)
sd(d$FORMALED, na.rm = T)
