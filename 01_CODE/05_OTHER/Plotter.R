
filename <- "BestFR_TIC_K7_Backward.png"
png(filename, width = 14.51, height = 9.92, units = "cm", res = 300)
par(mfrow=c(1,1))
#par(mar=c(1.5,1.5,1.5,1.5))

plot(Best_Solutions$Results, ylab = "Best Fisher Ratio", xlab = "Index", 
     main="Stepwise Backward Variable Selection.\nTIC. Best Fisher ratios.", pch = 20)

dev.off()