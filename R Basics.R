
######Introduction#####

0.15*19.71
install.packages("dslabs")
library(dslabs)
install.packages("tidyverse")
library(tidyverse)
data("murders")
murders %>%
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()
a <- 2
b <- -1
c <- -4

(-b+sqrt(b^2-4*a*c))/(2*a)
(-b-sqrt(b^2-4*a*c))/(2*a)

a
print(b)
ls()
x


ls
ls()

log(8)
log(a)
exp(a)
log(exp(1))

help("log")
?log


args(log)
log(8,base=2)
log(8)

log(8,2)

2^3
?"+"
  
data()
co2

pi

Inf

# To include comments

#####Data Types#####

class(a)

library(dslabs)
data("murders")
class("murders")
class(murders)

str(murders)

head(murders)

names(murders)

pop <- murders$population
length(pop)
class(pop)

class(murders$state)


z <- 3==2
z

class(z)

#Factors are integers, they are store in small numbers (integers), to be more efficient
class(murders$region)

######Section 1 Assessment######

a <- 2
b <- -1
c <- -4

(-b+sqrt(b^2-4*a*c))/(2*a)
(-b-sqrt(b^2-4*a*c))/(2*a)

log(1024,4)


install.packages("dslabs")
library(dslabs)
data(movielens)

class(movielens$title)

class(movielens$genres)

nlevels(movielens$genres)

#####VEctors#####


codes <- c(italy=380,canada=124,egypt=818)
codes
class(codes)

country <- c("italy","canada","egypt")
names(codes) <- country

seq(1,77,7)

codes
codes [2]
codes [c(1,3)]

codes["canada"]


#####Vectors coercion#####

x <- c(1,"canada", 2)
x
class(x)

as.character(x)
as.numeric(x)


#####Sorting#####


library(dslabs)
data(murders)
sort(murders$total)
order(murders$total)

murders$state[1:10]

index <- order(murders$total)
murders$abb[index]
murders$state[index]

max(murders$total)
i_max <- which.max(murders$total)
i_max
murders$state[i_max]

rank(murders$total)

#####Vector Aritmetic#####

#Murders per capita


murders$state[which.max(murders$population)]
max(murders$population)

#heights in inches
heights <- c(69,62,66,70,70,73,67,73,67,70)
heights*2.54
heights-69


#murder rate

murder_rate <- murders$total/murders$population*100000
murder_rate

#State in decreasing order for murder rate

order_rate <- order(murder_rate, decreasing=TRUE)
order_rate

Decreasing_rate <- murders$state[order_rate]
Decreasing_rate 

######Section 2 Assessment######

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time_h <- time/60
time_h

speed <- distance/time_h

speed 




####Indexing:####

# defining murder rate as before
murder_rate <- murders$total / murders$population * 100000

# creating a logical vector that specifies if the murder rate in that state is less than or equal to 0.71
index <- murder_rate <= 0.71
index

# determining which states have murder rates less than or equal to 0.71
murders$state[index]

# calculating how many states have a murder rate less than or equal to 0.71
sum(index)

# creating the two logical vectors representing our conditions
west <- murders$region == "West"
safe <- murder_rate <= 1

# defining an index and identifying states with both conditions true
index <- safe & west
murders$state[index]

x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)    # returns indices that are TRUE

# to determine the murder rate in Massachusetts we may do the following
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas","Boston"), murders$state)
index
murders$state[index]
murder_rate[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x

# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state



#####Basic Data Wrangling#####

# installing and loading the dplyr package
install.packages("dplyr")
library(dplyr)

# adding a column with mutate
library(dslabs)
data("murders")
murders <- mutate(murders, rate = total / population * 100000)
murders

# subsetting with filter
filter(murders, rate <= 0.71)

# selecting columns with select
new_table <- select(murders, state, region, rate)
new_table
head(new_table)

# using the pipe
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)

grades




library(dplyr)
data(murders)


######Basic Plots######


library(dplyr)
library(dslabs)
data("murders")

# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)

# a histogram of murder rates
murders <- mutate(murders, rate = total / population * 100000)
hist(murders$rate)
murders$state[which.max(murders$rate)]

# boxplots of murder rates by region
boxplot(rate~region, data = murders)



#####The summarize function#####



library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# minimum, median, and maximum murder rate for the states in the West region
s <- murders %>% 
  filter(region == "West") %>%
  summarize(minimum = min(rate), 
            median = median(rate), 
            maximum = max(rate))
s

# accessing the components with the accessor $
s$median
s$maximum

# average rate unadjusted by population size
mean(murders$rate)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate


#####Summarizing with more than one value#####

library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# minimum, median, and maximum murder rate for the states in the West region using quantile
# note that this returns a vector
murders %>% 
  filter(region == "West") %>%
  summarize(range = quantile(rate, c(0, 0.5, 1)))


# returning minimum, median, and maximum as a data frame
my_quantile <- function(x){
  r <-  quantile(x, c(0, 0.5, 1))
  data.frame(minimum = r[1], median = r[2], maximum = r[3]) 
}
murders %>% 
  filter(region == "West") %>%
  summarize(my_quantile(rate))




#####Pull to access columns#####

library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# us_murder_rate is stored as a data frame
class(us_murder_rate)

# the pull function can return it as a numeric value
us_murder_rate %>% pull(rate)

# using pull to save the number directly
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  pull(rate)
us_murder_rate

# us_murder_rate is now stored as a number
class(us_murder_rate)




#####The dot placeholder#####

library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# using the dot to access the rate
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  .$rate
us_murder_rate
class(us_murder_rate)





#####Group then summarize#####

library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# group by region
murders %>% group_by(region)

# summarize after grouping
murders %>% 
  group_by(region) %>%
  summarize(median = median(rate))


#####Sorting data tables#####

library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# order the states by population size
murders %>% arrange(population) %>% head()

# order the states by murder rate - the default is ascending order
murders %>% arrange(rate) %>% head()

# order the states by murder rate in descending order
murders %>% arrange(desc(rate)) %>% head()

# order the states by region and then by murder rate within region
murders %>% arrange(region, rate) %>% head()

# return the top 10 states by murder rate
murders %>% top_n(10, rate)

# return the top 10 states ranked by murder rate, sorted by murder rate
murders %>% arrange(desc(rate)) %>% top_n(10)



######Introduction to data.table#####

# install the data.table package before you use it!
install.packages("data.table")

# load data.table package
library(data.table)

# load other packages and datasets
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)

# convert the data frame into a data.table object
murders <- setDT(murders)

# selecting in dplyr
select(murders, state, region)

# selecting in data.table - 2 methods
murders[, c("state", "region")] |> head()
murders[, .(state, region)] |> head()

# adding or changing a column in dplyr
murders <- mutate(murders, rate = total / population * 10^5)

# adding or changing a column in data.table
murders[, rate := total / population * 100000]
head(murders)
murders[, ":="(rate = total / population * 100000, rank = rank(population))]

# y is referring to x and := changes by reference
x <- data.table(a = 1)
y <- x

x[,a := 2]
y

y[,a := 1]
x

# use copy to make an actual copy
x <- data.table(a = 1)
y <- copy(x)
x[,a := 2]
y


#####Subsetting with data.table#####



# load packages and prepare the data
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
library(data.table)
murders <- setDT(murders)
murders <- mutate(murders, rate = total / population * 10^5)
murders[, rate := total / population * 100000]

# subsetting in dplyr
filter(murders, rate <= 0.7)

# subsetting in data.table
murders[rate <= 0.7]

# combining filter and select in data.table
murders[rate <= 0.7, .(state, rate)]

# combining filter and select in dplyr
murders %>% filter(rate <= 0.7) %>% select(state, rate)


#####Summarizing with data.table#####


# load packages and prepare the data - heights dataset
library(tidyverse)
library(dplyr)
library(dslabs)
data(heights)
heights <- setDT(heights)

# summarizing in dplyr
s <- heights %>% 
  summarize(average = mean(height), standard_deviation = sd(height))

# summarizing in data.table
s <- heights[, .(average = mean(height), standard_deviation = sd(height))]

# subsetting and then summarizing in dplyr
s <- heights %>% 
  filter(sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))

# subsetting and then summarizing in data.table
s <- heights[sex == "Female", .(average = mean(height), standard_deviation = sd(height))]

# previously defined function
median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}

# multiple summaries in data.table
heights[, .(median_min_max(height))]

# grouping then summarizing in data.table
heights[, .(average = mean(height), standard_deviation = sd(height)), by = sex]


#####Sorting data frames#####

# load packages and datasets and prepare the data
library(tidyverse)
library(dplyr)
library(data.table)
library(dslabs)
data(murders)
murders <- setDT(murders)
murders[, rate := total / population * 100000]

# order by population
murders[order(population)] |> head()

# order by population in descending order
murders[order(population, decreasing = TRUE)] 

# order by region and then murder rate
murders[order(region, rate)]



#####Section 3 Assessment#####

library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

#Question 1:
avg <- mean(heights$height)
ind <- filter(heights, heights$height>mean(heights$height))
nrow(ind)


ind <- heights$height>mean(heights$height)
ind
heights$height[ind]
sum(ind)


#Question 2

avg <- mean(heights$height)
ind <- filter(heights, height>mean(height)& sex=="Female")
nrow(ind)


ind <- heights$height>mean(heights$height) & heights$sex=="Female"
ind
heights$height[ind]
sum(ind)


####

#Question 3

avg <- mean(heights$height)
ind_f <- filter(heights, sex=="Female")
ind_m <- filter(heights, sex=="Male")
nrow(ind_f)/(nrow(ind_f)+nrow(ind_m))


ind_f <- heights$sex=="Female"
ind_m <- heights$sex=="Male"
sum(ind_f)/(sum(ind_f)+sum(ind_m))
#Consejo (1 de 1):If you use mean() on a logical (TRUE/FALSE) vector, 
#it returns the proportion of observations that are TRUE.
mean(ind_f)


#Question 4
#a
which.min(heights$height)
heights$height[which.min(heights$height)]
#b
match(50,heights$height)
#c
heights$sex[which.min(heights$height)]
heights$sex[1032]


#Question 5
#a
which.max(heights$height)
heights$height[which.max(heights$height)]
#b
x <- 50:82
#c
x %in% heights$height
sum(!x %in% heights$height)


#Question 6
#a
ht_cm <- heights$height*2.54
ht_cm
heights2 <- data.frame(heights,ht_cm)
heights2$ht_cm[18]
#b
mean(heights2$ht_cm)


#Question 7
#a
sum(heights2$sex=="Female")
#b
mean(heights2$ht_cm[heights2$sex=="Female"])

#Question 8

library(dslabs)
data(olive)
head(olive)

plot(olive$palmitic,olive$palmitoleic)

#Question 9
hist(olive$eicosenoic)

#Question 10

# boxplots of murder rates by region
boxplot(palmitic~region, data = olive)



#####Programming Basics...#####

#####Basic Conditionals#####

# an example showing the general structure of an if-else statement
a <- 0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

# an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}

# changing the condition to < 0.25 changes the result
if(murder_rate[ind] < 0.25){
  print(murders$state[ind]) 
} else{
  print("No state has a murder rate that low.")
}

# the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA)

# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)
result

# the ifelse() function is also helpful for replacing missing values
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)


#####Functions Basic Function#####

# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

# we see that the above function and the pre-built R mean() function are identical
x <- 1:100
identical(mean(x), avg(x))

# variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s

# the general form of a function
#my_function <- function(VARIABLE_NAME){
#  perform operations on VARIABLE_NAME and calculate VALUE
#  VALUE
#}

# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}



#####For Loops#####

# creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

# a very simple for-loop
for(i in 1:5){
  print(i)
}

# a for-loop for our summation
m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

# creating a plot for our summation function
n <- 1:m
plot(n, s_n)

# a table of values comparing our function to the summation formula
head(data.frame(s_n = s_n, formula = n*(n+1)/2))

# overlaying our function with the summation formula
plot(n, s_n)
lines(n, n*(n+1)/2)



#####Other Functions#####


#####Section 4 Assessment#####

library(dslabs)
data(heights)

#Question 1

vector_sex <- vector(length = length(heights$sex))

vector_sex <- ifelse(heights$sex=="Female",1,2)

sum(vector_sex)

#Question 2

vector_height <- ifelse(heights$height>72,heights$height,0)

mean(vector_height)

#Question 3

inches_to_ft <- function(x){
  s <- x/12
  s
}
inches_to_ft(144)


vector_5 <- ifelse(inches_to_ft(heights$height)<5,1,0)

sum(vector_5)

#Question 4

any(TRUE, TRUE, TRUE)

any(TRUE, TRUE, FALSE)

any(TRUE, FALSE, FALSE)

any(FALSE, FALSE, FALSE)

all(TRUE, TRUE, TRUE)

all(TRUE, TRUE, FALSE)

all(TRUE, FALSE, FALSE)

all(FALSE, FALSE, FALSE)



#Question 5

# define a vector of length m
m <- 2
f_n <- vector(length = m)

# make a vector of factorials
for(n in 1:m) {
  f_n[n] <- factorial(n)
}

# inspect f_n
f_n




