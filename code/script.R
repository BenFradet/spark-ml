data <- read.csv("data.csv")

n <- 1000
shw_sampled <- data[sample(1 : nrow(data), n, replace = FALSE),
            c("SEX", "height", "weight")]
shw <- shw_sampled[shw_sampled$weight < 600 & shw_sampled$height < 80, ]

km <- kmeans(shw[, c("height", "weight")], 2, algorithm = "MacQueen")

shw$cluster <- km$cluster

filename <- "before"
png(paste(filename, ".png", sep = ""))
plot(shw$height, shw$weight,
     pch = 3,
     xlab = "Height in inches",
     ylab = "Weight in pounds")
dev.off()
svg(paste(filename, ".svg", sep = ""))
plot(shw$height, shw$weight,
     pch = 3,
     xlab = "Height in inches",
     ylab = "Weight in pounds")
dev.off()

filename <- paste("kmeans1", sep = "")
png(paste(filename, ".png", sep = ""))
plot(shw$height, shw$weight,
     pch = c(1, 3)[shw$cluster],
     xlab = "Height in inches",
     ylab = "Weight in pounds")
dev.off()
svg(paste(filename, ".svg", sep = ""))
plot(shw$height, shw$weight,
     pch = c(1, 3)[shw$cluster],
     xlab = "Height in inches",
     ylab = "Weight in pounds")
dev.off()

filename <- paste("kmeans2", sep = "")
png(paste(filename, ".png", sep = ""))
plot(shw$height, shw$weight,
     pch = c(3, 1)[shw$cluster],
     xlab = "Height in inches",
     ylab = "Weight in pounds")
dev.off()
svg(paste(filename, ".svg", sep = ""))
plot(shw$height, shw$weight,
     pch = c(3, 1)[shw$cluster],
     xlab = "Height in inches",
     ylab = "Weight in pounds")
dev.off()

filename <- "real"
png(paste(filename, ".png", sep = ""))
plot(shw$height, shw$weight,
     pch = c(1, 3)[shw$SEX],
     xlab = "Height in inches",
     ylab = "Weight in pounds")
dev.off()
svg(paste(filename, ".svg", sep = ""))
plot(shw$height, shw$weight,
     pch = c(1, 3)[shw$SEX],
     xlab = "Height in inches",
     ylab = "Weight in pounds")
dev.off()
