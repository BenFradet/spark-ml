library(ggplot2)

poisson_plot <- function() {
    p <- ggplot(transform(data.frame(x=c(0:5)), y=dpois(x, 0.75)), aes(x, y)) +
        geom_bar(stat="identity") +
        ggtitle("Poisson distribution for lambda = 0.75") +
        labs(x = "Number of occurrences", y = "Probability") +
        theme(plot.title = element_text(hjust = 0.5))
    return(p)
}

png("c5_poisson.png")
print(poisson_plot())
dev.off()
svg("c5_poisson.svg")
print(poisson_plot())
dev.off()
pdf("c5_poisson.pdf")
print(poisson_plot())
dev.off()
