library('rlist')
all.currs <- list()

boats.df <- read.csv("./data/boat_data.csv")

for (c in boats.df$Price)
{
  splitted <- str_split_fixed(c, " ", 2)
  curr <- splitted[1]
  value <- splitted[2]
  all.currs <- list.append(all.currs, curr)
}

u.currs <- unique(all.currs)

library(hash)

h <- hash()
for (ac in all.currs) {
  if (ac == 'CHF') {
    h[[ac]] <- 1.01
  }
  if (ac == 'EUR') {
    h[[ac]] <- 1.0
  }
  if (ac == 'DKK') {
    h[[ac]] <- 0.13
  }
  if (ac == 'Ă‚ÂŁ') {
    h[[ac]] <- 1.15
  }
}

for (c in boats.df$Price)
{
  splitted <- str_split_fixed(c, " ", 2)
  curr <- splitted[1]
  value <- splitted[2]
  boats.df$Price[boats.df$Price == c] <- as.double(value) * h[[curr]]
  # return()
}

# unify_currency <- function(df_x){
#  if (){
#
#  }
# }