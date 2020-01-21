# Task 1.2
###
# Use the rep() function to create a logical vector starting with three true values,
# then four false values, followed by two true values and finally five false values.
# Assign this logical vector to the variable x. 
# Finally, convert this vector onto the numeric vector.
# How have true and false values changed?

x <- rep(c(TRUE, FALSE, TRUE, FALSE), c(3, 4, 2, 5))
x_numeric <- as.numeric(x)

# Task 1.3
###
# A palindrome is a vector whose elements read from the end
# form the same vector as those read from the beginning.
# create such a vector of 100 numbers, where the first 20 numbers 
# are consecutive natural numbers, then there are 10 zeros,
# then 20 consecutive even numbers, and the remaining elements are determined
# by palindromicity (symmetry condition).

consecutive_numbers <- c(1:20)
zeros <- c(1:10) * 0
consecutive_evens <- c(1:20) * 2
first_half <- c(consecutive_numbers, zeros, consecutive_evens)
second_half <- rev(first_half)
palindrome <- c(first_half, second_half)

# Task 1.4
###
# From vector letters, choose letters at positions 5, 10, 15, 20, 25.

y = c(letters[5], letters[10], letters[15], letters[20], letters[25])

# Task 1.5
###
# Create a vector x of natural numbers from 1 to 1000, 
# and then replace even numbers to their inverse.

vec <- c(1:1000)
evens <- vec[vec %% 2 == 0]
inverses <- c(1) / evens

# Task 1.6
###
# Order the elements of the vector  
# (6, 3, 4, 5, 2, 3)
# from largest to smallest using the function order().

vector <- c(6, 3, 4, 5, 2, 3)
sorted <- vector[order(vector, DECREASING = TRUE)]

# Task 1.7
###
# Calculate the signs of vector elements  
# (−1.876, −1.123, −0.123, 0, 0.123, 1.123, 1.876).
# Then round the elements of this vector to two decimal points.
# At the end, calculate the floor of each element of the new vector.

elements <- c(-1.876, -1.123, -0.123, 0, 0.123, 1.123, 1.876)
signs <- sign(elements)
rounded <- round(elements, digits = 2)
floored <- floor(elements)

# Task 1.8
###
# Calculate the square root of each natural number from 1 to 100 million
# in two ways. First use built-in function in R, and then use exponentiation.
# Which way works faster?
# Hint: You can use the Sys.time() function to check the length of time the program has run.

z <- c(1:100000000)

before <- Sys.time()
squared <- sqrt(z)
after <- Sys.time()
time <- after - before

square <- function(x) { x ^ (1/2) }

before <- Sys.time()
squared <- lapply(z, square(z))
after <- Sys.time()
time <- after - before

# Task 1.9
###
# Calculate the sum, product, arithmetic mean, variance, standard deviation, median
# and quantiles of orders  (0, 1/4, 1/2, 3/4, 1) of a vector  
# (1, 2, ... , 10). What are the quantiles of order 0 and  1?

v <- c(1:10)
sum <- sum(v)
prod <- prod(v)
mean <- mean(v)
variance <- var(v)
sd <- sd(v)
med <- median(v)
q <- quantile(v)
min <- q[1]
max <- q[5]

# Task 1.10
###
# The schoolmath package contains the primlist data set, 
# which contains prime numbers between 1 and 9999999.
# Find the largest prime number less than 1000.
# How many prime numbers are greater than 100 and less than 500?

install.packages("schoolmath")
library(schoolmath)
data(primlist)

largest <- max(primlist[primlist <= 1000])
count <- length(primlist[primlist > 100 & primlist < 500])

# Task 1.11
###
# Determine all combinations of vectors  
# (a, b) and  (1, 2, 3)
# using the rep() and paste() functions.

combinations <- paste(rep(v1, each = 3), v2, sep = "")

