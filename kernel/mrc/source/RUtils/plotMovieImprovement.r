data <- read.table("SCRATCH/movie_data.txt", header=T)

pdf(file="movie_factor.pdf")

hist(data$Factor, main="Improvement Factor", prob=T)
lines(density(data$Factor, bw=0.5), col="red", lwd=2)

dev.off()
