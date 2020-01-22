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

### TASK 12 ###
# Create a vector of 30 strings of the form number.letter, where the number is consecutive natural numbers
# from 1 to 30, and the letter is three capital letters X, Y, Z occuring cyclically.
number <- c(1:30)
letter <- c('X', 'Y', 'Z')
paste(number, letter, sep=".")


### TASK 13 ###
# In some situations, it may be useful to categorize variable, i.e., a different division into categories 
# than would result from the data.
#   Generate 100 observations that are the answers to the survey questions, each answer can take one of 
#   the values: 'a', 'b', 'c', 'd', 'e'.
#   Categorize the obtained observations so that category 1 includes 'a' and 'b' responses, category 2 
#   'c' and 'd' responses, and category 3 'e' response.
# Hint: Use the sample() function and the recode() function from the car package.
install.packages("car", dependencies = TRUE)
require(car) # or library(car)
library(help = "car")
# help('sample')
obv <- rep(sample(c('a', 'b', 'c', 'd', 'e')), length.out = 100)
# help('recode')
encode <- recode(obv, "c('a','b')=1;c('c','d')=2;'e'=3")


### TASK 14 ###
# Create the vector x of elements NA, 3, 14, NA, 33, 17, NA, 41.
#   Count the number of missing values.
#   Calculate the arithmetic mean without taking into account the missing values.
#   Remove missing data.
#   Replace the missing values with 11.
x <- c(NA, 3, 14, NA, 33, 17, NA, 41)
sum(as.numeric(is.na(x)))
mean(x, na.rm=TRUE)
# na.omit(x) or:
indices <- which(is.na(x))
x[-indices]
replace(x, indices, 11)


### TASK 15 ###
# Create a list called my_list, whose first element will be a two-element character vector containing your 
# first name and surname, the second element will be the number π, the third the unique() function, and 
# the last element of the list will be a vector consisting of numbers 0.1,0.2,…,1. Then remove the first 
# element and the third element from this list. Finally, create a list containing values of the gamma() 
# function for the elements of the my_list object.
name = c('Jan', 'Kowalski')
x = seq(0.1, 1, by = 0.1)
func <- list(unique)
my_list <- list(name, pi, func, x)
my_list[c(1, 3)] <- NULL
my_list_gamma <- list(gamma(my_list[[1]]), gamma(my_list[[2]]))
