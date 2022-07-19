hist(vball$vj)
hist(vball$height)
hist(vball$frequency)

summary(vball$vj)
summary(vball$height)
summary(vball$frequency)

psych::alpha(vball$vj)
psych::alpha(vball$height)
psych::alpha(vball$frequency)

vballPOS.df <- data.frame(vball$poscon, vball$jumpcon)
vballNEG.df <- with(vball, data.frame(vball$negcon))
vballRNEG.df <- 4-vballNEG.df
CONFIDENCE.df <- cbind(vballPOS.df, vballRNEG.df)
psych::alpha(CONFIDENCE.df)
vball$CONFIDENCE <- rowMeans(CONFIDENCE.df, na.rm = T)
hist(vball$CONFIDENCE, main = "Confidence Levels of Volleyball Players")

heightmod <- lm (vj ~ height, data = vball)
freqmod <- lm (vj ~ frequency, data = vball)
CONmod <- lm (vj ~ CONFIDENCE, data = vball)

plot(vj ~ height, data = vball, main = "Height on Vertical Jump", xlab = "Height", ylab = "Vertical Jump")
plot(vj ~ frequency, data = vball, main = "Training Frequency on Vertical Jump", xlab = "Frequency", ylab = "Vertical Jump")

plotmeans(vj ~ height, data = vball, main = "Height on Vertical Jump", xlab = "Height", ylab = "Vertical Jump")
plotmeans(vj ~ frequency, data = vball, main = "Training Frequency on Vertical Jump", xlab = "Frequency", ylab = "Vertical Jump")
plotmeans(vj ~ CONFIDENCE, data = vball, main = "Confidence and Vertical Jump", xlab = "Confidence Level", ylab = "vertical Jump")

coef(heightmod)
coef(freqmod)
coef(CONmod)
summary(heightmod)$r.squared
summary(freqmod)$r.squared
summary(CONmod)$r.squared

bucket <- array()
for(i in c(1:1000)){
  vballX <- vball[sample(1:nrow(height), nrow(height),replace = T)]
  heightmodX <- lm(vj ~ height, vballX)
  bucket[i] <- coef (heightmodX)[2]
}
sd(bucket)
coef(heightmodX)[2] + 1.96*sd(bucket)
coef(heightmodX)[2] - 1.96*sd(bucket)
summary(heightmodX)

summary(heightmod)
summary(freqmod)
summary(CONmod)