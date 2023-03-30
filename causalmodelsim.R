####################################
### Causal model and simulation
### for "Does educational attainment reduce religiosity?"
### by Benjamin Grant Purzycki and Theiss Bendixen
###########################################

# This section inspired by https://www.felixthoemmes.com/blog/the-front-door-criterion-in-linear-parametric-models/

library(dagitty)

mycol1 <- rgb(246, 246, 246, max = 255, alpha = 100, names = "white")
mycol2 <- rgb(187, 187, 187, max = 255, alpha = 100, names = "lightgray") 
mycol3 <- rgb(128, 128, 128, max = 255, alpha = 100, names = "darkgray")
mycol4 <- rgb(60, 60, 60, max = 255, alpha = 125, names = "darkgray")
mycol5 <- rgb(0, 0, 0, max = 255, alpha = 150, names = "darkgray")

fd <- function(n, beta) {
  e_rel <- rnorm(n, 0, 1) # noise
  e_edu <- rnorm(n, 0, 1)
  e_mat <- rnorm(n, 0, 1)
  e_qua <- rnorm(n, 0, 1)
  e_sex <- rnorm(n, 0, 1)
  e_age <- rnorm(n, 0, 1) 
  e_kid <- rnorm(n, 0, 1)
  SEX <- rbinom(n, 1, .5) # sex
  XIA <- rbinom(n, 1, .5) # edu type
  PRI <- rbinom(n, 1, .5) # prime
  #AGE <- beta * SAM + e_age
  AGE <- rnorm(n, 0, 1)
  EDU <- beta * SEX + beta * AGE + e_edu
  MAT <- beta * SEX * beta * EDU + beta * AGE + e_mat
  KID <- beta * MAT + beta * AGE + e_kid # children
  #REL <- beta * EDU + beta * XIA + beta * MAT + beta * SEX + beta * AGE + 
  #  beta * SAM + beta * PRI + e_rel # religiosity
  REL <- beta * EDU + beta * XIA + beta * MAT + beta * SEX + beta * AGE + 
    beta * PRI + beta * KID + e_rel # religiosity
  #df <- data.frame(SEX, XIA, PRI, SAM, AGE, EDU, MAT, REL)
  df <- data.frame(SEX, XIA, PRI, AGE, EDU, MAT, REL)
  open0 <- coef(lm(REL ~ EDU, dat = df))[2]
  open1 <- coef(lm(REL ~ EDU + SEX, dat = df))[2]
  open2 <- coef(lm(REL ~ EDU + SEX + MAT, dat = df))[2]
  open3 <- coef(lm(REL ~ EDU + SEX + MAT + AGE + KID, dat = df))[2]
  closed <- coef(lm(REL ~ EDU + SEX + MAT + AGE + + KID + XIA, dat = df))[2]
  return(c(open0, open1, open2, open3, closed))
}

sim1 <- data.frame(t(replicate(1000, fd(1000, .5))))
names(sim1) <- c("open0", "open1", "open2", "open3", "controlled")

densop0 <- density(sim1$open0)
densop1 <- density(sim1$open1)
densop2 <- density(sim1$open2)
densop3 <- density(sim1$open3)
densco1 <- density(sim1$controlled)

par(mfrow = c(2, 1), mar = c(2, 1, 1, 1))

plot(dagitty('dag {
bb="0,0,1,1"
"Education (Amount)" [exposure,pos="0.226,0.637"]
"Education (Type)" [latent,pos="0.206,0.471"]
"Food Security" [adjusted,pos="0.363,0.297"]
Age [adjusted,pos="0.492,0.507"]
Children [adjusted,pos="0.220,0.297"]
PRIME [pos="0.494,0.637"]
Religiosity [outcome,pos="0.363,0.637"]
Sex [adjusted,pos="0.495,0.385"]
"Education (Amount)" -> "Food Security"
"Education (Amount)" -> Religiosity
"Education (Type)" -> Religiosity
Children -> "Food Security" 
"Food Security" -> Religiosity
Age -> "Education (Amount)"
Age -> "Food Security"
Age -> Children
Age -> Religiosity
Children -> Religiosity
PRIME -> Religiosity
Sex -> "Education (Amount)"
Sex -> "Food Security"
Sex -> Religiosity
}
           ')) 

plot(NA, xlab = NA, ylab = "", 
     xlim = c(0.3, 1.3), 
     ylim = c(0, 13), 
     cex.lab = 1.3, yaxt = 'n')
polygon(densop0, col = mycol1) # open0
polygon(densop1, col = mycol2) # open1
polygon(densop2, col = mycol3) # open2
polygon(densop3, col = mycol4) # open3
polygon(densco1, col = mycol5) # closed
abline(v = 0.5, lty = 2)
legend(1.1, 13, legend = c("~ Edu. (Amnt.)", "+ Sex", "+ Food Sec.", "+ Age + Kids",
                           "+ Edu (Type)"), 
       fill = c(mycol1, mycol2, mycol3, mycol4, mycol5), 
       cex = .8, horiz = F, bty = T, inset = c(0.03, 0.15)) 

