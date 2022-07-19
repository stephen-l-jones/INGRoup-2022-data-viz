# Use ?<function name> or help("<function name>") to see the help for a function.
?read.csv
help("read.csv")

# Use <- to assign a value, data structure, function, etc. to a variable
nba_wins <- read.csv("data/nba_salary_wins.csv")

# Use <package>::<function name>() to specify the package with the function. This is useful
# when two packages have differing functions with the same name.
nba_wins <- utils::read.csv("data/nba_salary_wins.csv")

# Use library() to load a package's functions for use. You can use a function without loading
# the package by using the <package>:: prefix as long as the package is installed from 
# CRAN or github. Functions in a few core packages (e.g., base, utils) are always available.
install.packages("ggthemes")
show_shapes(1:25)
ggthemes::show_shapes(1:25)
library(ggthemes)
show_shapes(1:25)

# R has six basic data types. The four most common are logical, numeric, integer, and character.
class(TRUE)
class(5)
class(5L)
class("Hello world!")

# The four most common data structures are: vector, matrix, list, data.frame. 
# The elements of a vector or matrix must all by the same type (e.g., logical, numeric).
my_vector <- c(1,5,7,9)
my_vector
length(my_vector)

my_matrix <- matrix(1:12, 3, 4)
my_matrix
dim(my_matrix)

# The elements of a list can be completely different from one another.
my_list <- list(elem1 = matrix(1:12, 3, 4), elem2 = LETTERS[1:8], elem3 = "ABC")
my_list
length(my_list)
names(my_list)

# The columns of a data.frame can also differ, but each column must be the same length.
my_data.frame <- data.frame(col1 = LETTERS[1:8], col2 = 1:8, col3 = "ABC")
my_data.frame
dim(my_data.frame)
names(my_data.frame)

# If needed and reasonable, data.frame will recycle the values of a shorter column to match the
# length of longer columns.
data.frame(long_col = 1:10, short_col = LETTERS[1:2])
data.frame(long_col = 1:10, short_col = LETTERS[1:3])

# Elements of a data structure can be accessed by index, logical vector, or name (if given).
my_matrix <- matrix(1:12, 3, 4)
my_matrix
my_matrix[2,]
my_matrix[,2]
my_matrix[2,2]
my_matrix[c(TRUE,FALSE,TRUE),]
my_matrix[, colSums(my_matrix) < 20]

my_list <- list(elem1 = matrix(1:12, 3, 4), elem2 = LETTERS[1:8], elem3 = "ABC")
my_list[2:3]
my_list[[2]]
my_list[["elem2"]]

my_data.frame <- data.frame(col1 = LETTERS[1:8], col2 = 1:8, col3 = "ABC")
my_data.frame
my_data.frame[, c("col1","col2")]
my_data.frame$col1

# You can also assign new values use the same reference syntax and, except for matrices,
# to create new columns or elements.
my_matrix <- matrix(1:12, 3, 4)
my_matrix
my_matrix[3,] <- my_matrix[1,] * 10
my_matrix

my_data.frame <- data.frame(col1 = LETTERS[1:8], col2 = 1:8, col3 = "ABC")
my_data.frame
my_data.frame$col4 <- "DEF"
my_data.frame

# The parameters in a function must be given in order; otherwise, the parameter names must be 
# explicitly given.
?sample
sample(1:10, 5, TRUE)
sample(1:10, 5, prob = seq(.1, 1, .1))

# The dot-dot-dot definition accepts multiple user-defined parameters.
?base::paste
paste(c("R","T"), 1:4, sep = ".")
paste(c("R","T"), 1:4, sep = ".", collapse = "|")

# The tidyverse is a suite of packages that all work with the same philosophy. The most important
# of these is dplyr, magrittr, and tidyr. But there are many others packages in the tidyverse.
# magrittr has a pipe operator %>%, which chains functions together. The following
# do the same thing.
mean(pull(dplyr::filter(read.csv("data/nba_salary_wins.csv"), year == 2014), pct_win))

read.csv("data/nba_salary_wins.csv") %>%
  dplyr::filter(year == 2014) %>%
  pull(pct_win) %>%
  mean()
  
# dplyr has a modified data.frame class called tibble. It is essentially a data.frame but
# prints a bit nicer.
nba_wins <- read.csv("data/nba_salary_wins.csv")
class(nba_wins)
nba_wins <-  as_tibble(nba_wins)
class(nba_wins)
nba_wins

# dplyr has useful functions for manipulating data frames. The following are equivalent.
nba_wins <- read.csv("data/nba_salary_wins.csv") %>% as_tibble()

nba_wins[nba_wins$season == "2013-14",]       # base R
nba_wins %>%                                  # dplyr
  dplyr::filter(season == "2013-14") 

nba_wins[, c("team_code","salary_team")]       # base R
nba_wins %>%                                   # dplyr
  select(team_code, salary_team) 

nba_wins[, "salary_team"] <- mean(nba_wins$salary_team)  # base R
nba_wins %>%                                             # dplyr
  mutate(mean_salary_team = mean(salary_team))


do.call(rbind, lapply(split(nba_wins, nba_wins$season),  # base R
                      function(x) tibble(season = x$season[1], mean_season = mean(x$salary_team))))
nba_wins %>%                                             # dplyr
  group_by(season) %>%
  summarise(mean_season = mean(salary_team))

nba_wins[order(-nba_wins$salary_team),]       # base R
nba_wins %>%                                  # dplyr
  arrange(desc(salary_team))

# tidyr has useful functions for transforming data from long to wide format or vice-versa
nba_wins_wide <- nba_wins %>%
  dplyr::filter(year %in% 2014:2015) %>%
  pivot_wider(id_cols = team_code, names_from = year, values_from = c(salary_team, pct_win))
nba_wins_wide

nba_wins_long <- nba_wins_wide %>%
  pivot_longer(!team_code, 
               names_to = c(".value","year"), 
               names_pattern = "(.*)_([[:digit:]]{4})")
nba_wins_long
