data <- read.table("SCRATCH/movie_data.txt", header=T)

pdf(file="movie_factor.pdf")

data2 <- 100*(data$Factor-1)

hist(data2, main="Improvement Factor", prob=T)
lines(density(data2, bw=0.5), col="red", lwd=2)


dev.off()
