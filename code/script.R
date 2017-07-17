data <- read.csv("data.csv")

n <- 50
shw_sampled <- data[sample(1 : nrow(data), n, replace = FALSE),
            c("SEX", "height", "weight")]
shw <- shw_sampled[shw_sampled$weight < 600 & shw_sampled$height < 80, ]

km <- kmeans(shw[, c("height", "weight")], 2, algorithm = "MacQueen")

shw$cluster <- km$cluster

save_images <- function(filename, df, points) {
  x_lab = "Height in inches"
  y_lab = "weight in pounds"
  png(paste(filename, ".png", sep = ""))
  plot(df$height, df$weight,
       pch = points, xlab = x_lab, ylab = y_lab, col = c(1, 2)[shw$SEX])
  dev.off()
  svg(paste(filename, ".svg", sep = ""))
  plot(df$height, df$weight,
       pch = points, xlab = x_lab, ylab = y_lab)
  dev.off()
  pdf(paste(filename, ".pdf", sep = ""))
  plot(df$height, df$weight,
       pch = points, xlab = x_lab, ylab = y_lab)
  dev.off()
}

save_images("c7_before", shw, 3)

save_images("c7_kmeans1", shw, c(1, 3)[shw$cluster])

save_images("c7_kmeans2", shw, c(3, 1)[shw$cluster])

save_images("c7_real", shw, c(1, 3)[shw$SEX])
