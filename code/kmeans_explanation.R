x <- c(1, 2, 5, 3, 9, 2, 8, 7, 6, 6)
y <- c(7, 6, 3, 5, 8, 2, 6, 3, 9, 7)

n <- 10
black <- 1
red <- 2
circle <- 1
triangle <- 2
cross <- 3

df <- data.frame(x, y, forms = rep(cross, n), colors = rep(black, n))

save_images <- function(filename, df) {
  png(paste(filename, ".png", sep = ""))
  plot(df$x, df$y, pch = df$forms, col = df$colors, xlab = "x", ylab = "y")
  dev.off()
  svg(paste(filename, ".svg", sep = ""))
  plot(df$x, df$y, pch = df$forms, col = df$colors, xlab = "x", ylab = "y")
  dev.off()
}

save_images("kmeans_before", df)

index1 <- 5
index2 <- 8
df$forms[index1] <- circle
df$forms[index2] <- triangle
df$colors[index1] <- red
df$colors[index2] <- red
save_images("kmeans_init", df)

dist <- function(x1, y1, x2, y2) {
  return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}

x1 <- df$x[index1]
y1 <- df$y[index1]
form1 <- circle
x2 <- df$x[index2]
y2 <- df$y[index2]
form2 <- triangle

df$forms <- apply(df, 1,
               function(row) {
                 d1 <- dist(row[1], row[2], x1, y1)
                 d2 <- dist(row[1], row[2], x2, y2)
                 if (d1 < d2) {
                   return(form1)
                 } else {
                   return(form2)
                 }
               })
save_images("kmeans_first", df)

x1 <- mean(df[df$forms == form1, "x"])
y1 <- mean(df[df$forms == form1, "y"])
x2 <- mean(df[df$forms == form2, "x"])
y2 <- mean(df[df$forms == form2, "y"])
df$colors <- rep(1, n)
df <- rbind(df, c(x1, y1, form1, red), c(x2, y2, form2, red))
save_images("new_center", df)
