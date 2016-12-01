x <- c(1, 2, 5, 3, 9, 2, 8, 7, 6, 6)
y <- c(7, 6, 3, 5, 8, 2, 6, 3, 9, 7)

n <- 10
circle <- 1
full_circle <- 16
triangle <- 2
full_triangle <- 17
cross <- 3

df <- data.frame(x, y, forms = rep(cross, n))

save_images <- function(filename, df) {
  png(paste(filename, ".png", sep = ""))
  plot(df$x, df$y, pch = df$forms, xlab = "x", ylab = "y")
  dev.off()
  svg(paste(filename, ".svg", sep = ""))
  plot(df$x, df$y, pch = df$forms, xlab = "x", ylab = "y")
  dev.off()
}

save_images("kmeans_before", df)

index1 <- 5
index2 <- 8
df$forms[index1] <- full_circle
df$forms[index2] <- full_triangle
save_images("kmeans_init", df)

dist <- function(x1, y1, x2, y2) {
  return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}

x1 <- df$x[index1]
y1 <- df$y[index1]
x2 <- df$x[index2]
y2 <- df$y[index2]

df$forms <- apply(df, 1,
               function(row) {
                 d1 <- dist(row[1], row[2], x1, y1)
                 d2 <- dist(row[1], row[2], x2, y2)
                 if (d1 < d2) {
                   return(circle)
                 } else {
                   return(triangle)
                 }
               })
df$forms[index1] <- full_circle
df$forms[index2] <- full_triangle
save_images("kmeans_first", df)

x1 <- mean(df[df$forms == circle, "x"])
y1 <- mean(df[df$forms == circle, "y"])
x2 <- mean(df[df$forms == triangle, "x"])
y2 <- mean(df[df$forms == triangle, "y"])
df$forms[index1] <- circle
df$forms[index2] <- triangle
df <- rbind(df, c(x1, y1, full_circle), c(x2, y2, full_triangle))
save_images("kmeans_new_center", df)
