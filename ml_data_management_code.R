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




#
#
# Subsetting a dataset
#
#






#
#
# Binding data together from various cohorts / surveys
#
#






