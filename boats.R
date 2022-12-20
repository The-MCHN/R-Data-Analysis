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

unified.values <- list()

for (c in 1:nrow(boats.df))
{
  splitted <- str_split_fixed(boats.df[c,]$Price, " ", 2)
  value <- splitted[2]
  curr <- splitted[1]
  print(as.double(splitted[2]))
  print(h[[curr]])
  print(as.double(splitted[2])*h[[curr]])
  unified.values <- list.append(unified.values, as.double(splitted[2])*h[[curr]])


}
print(unique(unified.values))

boats.df$Price<-unified.values

print(head(boats.df$Location))

for (c in 1:nrow(boats.df))
{
  splitted <- str_replace(boats.df[c,]$Location, "", "Ă»")
  # print(gsub("Ă‚Â»", ",", boats.df[c,]$Location))
  # print(boats.df[c,]$Location)
  # to.delete <- c('Ă', '>>')
  # boats.df$Location <- sub('Ă', "", boats.df$Location )
print(splitted[1])
  break

}
library(stringr)
boats.df$Location <- str_replace_all(boats.df$Location, "Ă»", "")