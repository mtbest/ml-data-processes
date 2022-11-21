# simulate data 

# set seed
set.seed(123)

# set sample size
n = 100

# simulate variables
id <- 1:n

gender <- sample(x = c("male", "female", "non-binary"), 
                 size = n,
                 replace = TRUE, 
                 prob = c(.4, .5, .1))

gpa <- round(rnorm(n = n, mean = 2.8, sd = .3), 2)

RaceEthnicity1 <- sample(x = c("White or Caucasian", 
                               "Black or African American", 
                               "Asian or Asian American"), 
                        size = n,
                        replace = TRUE, 
                        prob = c(.5, .3, .2))



GM_probabilities <- sample(x = seq(.1, .9, .01),
                           size = n,
                           replace = TRUE)

GM_1 <- rbinom(n = n, 
               size = 5,
               prob = x) + 1

GM_2 <- rbinom(n = n, 
               size = 5,
               prob = x) + 1

GM_3 <- rbinom(n = n, 
               size = 5,
               prob = x) + 1

GM_4_R <- 6 - rbinom(n = n, 
               size = 5,
               prob = x) + 1

# create data.frame
raw_df <- data.frame(id, 
                 gender, 
                 gpa,
                 RaceEthnicity1,
                 GM_1,
                 GM_2,
                 GM_3,
                 GM_4_neg)

# randomly replace values with NA
df <- as.data.frame(
  apply(raw_df, 
        1:2, 
        \(x) sample(c(x, NA), 
                   1, 
                   prob=c((1 - .1), .1))))


df$gpa2 <- df$gpa+1

# Now I'm doing this right (I think) - MB