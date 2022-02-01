## Honours quiz from week 1
## K. Garner & Angie Renton aka Rocky Rentz

# matrix of transition probs given chess
trans_mat <- matrix(c(0,1, 0.3, 0.7), nrow = 2, byrow = 2)
starting_position <- matrix(c(1, 0), nrow = 2)

# game 2
game2 <- trans_mat %*% starting_position

game2 <- trans_mat %*% trans_mat
game4 <- game2 %*% trans_mat


# finding the stationary distribution
n = 5000
x = numeric(n)
x[1] = 1
for (i in 2:n){
  x[i] = sample.int(2, size=1, prob=trans_mat[x[i-1],])
}
table(x)/n
