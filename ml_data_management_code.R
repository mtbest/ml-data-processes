#############################
##
## ML DATA MANAGEMENT CODE
##
#############################

#
#
# Merging data across time points
#
#

#
# Zach Method (built in studying the use case of needing to bind and merge 
# different datasets that had different elements of missing data in each)
#

library(tidyverse)

# create dummy data
x_pre <- data.frame(p = c(1, 2, 3), a = c(1, 2, 3), b = c(4, 5, 6), d = c(3, 4, 5))
x_post <- data.frame(p = c(1, 2, 3), a = c(5, 2, 9), c = c(4, 5, 6))
y_pre <- data.frame(p = c(4, 5, 6), b = c(1, 2, 3), c = c(4, 5, 6))
y_post <- data.frame(p = c(4, 5, 6), b = c(1, 2, 3), d = c(4, 5, 6), e = c(1, 2, 3))


# make function
my_fun <- function(df){
  df_name <- deparse(substitute(df))
  if (grepl("pre", df_name)) {
    colnames(df)[-1] <- paste(colnames(df)[-1], "t1", sep = "_") 
  }
  else if (grepl("post", df_name)) {
    colnames(df)[-1] <- paste(colnames(df)[-1], "t2", sep = "_")
  }
  return(df)
}

# create a list of data frames
my_list <- list(x_pre = x_pre, x_post = x_post,
                y_pre = y_pre, y_post = y_post)

# for loop through list
for (i in seq_along(my_list)) {
  df_name <- names(my_list)[i]
  if (grepl("pre", df_name)) {
    colnames(my_list[[i]])[-1] <- paste(colnames(my_list[[i]])[-1], "t1", sep = "_") 
  }
  else if (grepl("post", df_name)) {
    colnames(my_list[[i]])[-1] <- paste(colnames(my_list[[i]])[-1], "t2", sep = "_")
  }
}

# alternative approach (probably better)
# uses anonymous function and map
# re-create the initial list of data frames
my_list <- list(x_pre = x_pre, x_post = x_post,
                y_pre = y_pre, y_post = y_post)

my_list <- Map(
  function(list_df, list_names) {
    if (endsWith(list_names, "pre")) {
      names(list_df)[-1] <- paste(names(list_df)[-1], "t1", sep = "_")
    } 
    else if (endsWith(list_names, "post")) {
      names(list_df)[-1] <- paste(names(list_df)[-1], "t2", sep = "_")
    }
    list_df
  }, 
  my_list, 
  names(my_list)
)

#merge all data frames together
final_product <- my_list %>% 
  reduce(full_join)


#
# Matt Method (before Zach method)
#

## not shown here, but typically need to clean dataset before merging

merged_data <- data_pre %>% full_join(data_post, by = c("x"), suffix = c("_pre", "_post"))

## not shown here, but typically need to review the merged column and do additional cleaning


#
#
# Subsetting a dataset
#
#

# Selecting certain rows

subset <- data[c(22:60),]

# Selecting certain columns

subset <- data[,c(8:27)]

## This technique can be used to delete a set of columns

data_1 <- dplyr::select(data, -column1:-column18)

# Filtering for a certain response or group

newdata <- data %>% filter(var1 == 1 | var2 == 1)
newdata <- data %>% filter(var1 == 1 & var2 == 1)
newdata <- data %>% filter(var1 == "value" & var2 == "value")





#
#
# Binding data together from various cohorts / surveys
#
#

# Matt's very laborious way of binding data together

## Select all of the columns in the datasets to make sure they are all the same 
## and have the same names using dplyr::select(). Then, turn them all into data tables.

data1 <- as.data.table(data1)

## Next, fix all of the variable types to make sure the variable types in each dataset
## is aligned. For example...

data1$v1 <- as.character(data1$v1)
data1$v2 <- as.integer(data1$v2)
data2$v1 <- as.character(data2$v1)
data2$v2 <- as.integer(data2$v2)

## Then, bind the datasets

data <- funion(data1, data2)



