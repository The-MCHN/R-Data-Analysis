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
  # print(as.double(splitted[2]))
  # print(h[[curr]])
  # print(as.double(splitted[2]) * h[[curr]])
  unified.values <- list.append(unified.values, as.double(splitted[2]) * h[[curr]])
}
# print(unique(unified.values))

boats.df$Price <- unified.values

# print(head(boats.df$Location))

library(stringi)
for (c in 1:nrow(boats.df))
{
  splitted <- str_split_fixed(boats.df[c,]$Location, " ", 2)
  # print(splitted[1])
  boats.df[c,]$Location <- splitted[1]
  # print(boats.df[c,]$Location)
  # break

}

h.countries <- hash()
h.countries[["Croatia"]] <- c("Rovinij", "Kroatien", "Dalmatien", "Split", "Adria", "Novi")
h.countries[["Spain"]] <- c("Spain?a", "Ibiza", "Mallorca", "Marina", "Calanova", "Tenero,", "Spain?a", "espa?a")
h.countries[["Norway"]] <- c("NordseekĂ\u0083ÂĽste")
h.countries[["Denmark"]] <- c("Juelsminde")
h.countries[["Germany"]] <- c("Brandenburg", "Rostock", "Beilngries", "Bremen", "Berlin", "RĂ\u0083ÂĽgen", "Ostsee",
                              "Steinwiesen", "TravemĂ\u0083ÂĽnde", "Stralsund", "Greetsile/", "Niederrhein",
                              "Heilbronn", "Bielefeld", "Rheinfelden", "FuĂ\u0083Âźach", "Barssel")
h.countries[["Slovenia"]] <- c("Izola")
h.countries[["Switzerland"]] <- c("VierwaldstĂ\u0083Â¤ttersee", "baden", "Rolle", "Lake", "Faoug", "waren", "Neustadt",
                                  "ZĂ\u0083ÂĽrichse,", "Avenches", "Thalwil", "Welschenrohr", "Thun")
h.countries[["Portugal"]] <- c("PT", "lago", "Lago", "Porto")
h.countries[["Austria"]] <- c("Donau", "Neusiedl", "bodensee")
h.countries[["USA"]] <- c("United")
h.countries[["Netherlands"]] <- c("Zevenbergen", "Katwijk", "Wijdenes")
h.countries[["Poland"]] <- c("Oder")
h.countries[["Russia"]] <- c("Russian")
h.countries[["France"]] <- c("annecy")
h.countries[["Belgium"]] <- c("Opwijk", "Lommel", "BelgiĂ\u0083Â«,")
h.countries[["Italy"]] <- c("Toscana", "Angera", "Italien", "Italie")

for (key in keys(h.countries)) {
  for (name in h.countries[[key]]) {
    print(name)
    boats.df$Location <- str_replace_all(boats.df$Location, name, key)

  }
}

odd.countries <- c("83278", "Isle", "24782", "Spain?a")

for (c in 1:nrow(boats.df))
{
  splitted <- str_split_fixed(boats.df[c,]$Type, ",", 2)

  if (boats.df[c,]$Location %in% odd.countries)
  {
    boats.df$Location <- str_replace_all(boats.df$Location, boats.df[c,]$Location, "")

  }

}


library(stringr)
boats.df$Boat.Type <- str_replace_all(tolower(boats.df$Boat.Type), "boat", "")
boats.df$Boat.Type <- trimws(str_split_fixed(boats.df$Boat.Type, ',', 2)[, 1])

#fixing encoding errors
boats.df$Manufacturer <- str_replace_all(boats.df$Manufacturer, "BĂ\u0083Â©nĂ\u0083Â©teau power boats", "Beneteau")
boats.df$Manufacturer <- str_replace_all(boats.df$Manufacturer, "MĂ\u0083Â¤ndli power boats", "Maendli")
boats.df$Manufacturer <- str_replace_all(boats.df$Manufacturer, "FÃ¼llemann power boats", "Fuellemann")
boats.df$Manufacturer <- str_replace_all(boats.df$Manufacturer, "HervĂ\u0083Â© power boats", "Herve")
boats.df$Manufacturer <- str_replace_all(boats.df$Manufacturer, "LĂ\u0083ÂĽrssen Yachts power boats", "Luerssen Yachts")
boats.df$Manufacturer <- str_replace_all(boats.df$Manufacturer, "Ă\u0083Â–chsner power boats", "Oechsner")
boats.df$Manufacturer <- str_replace_all(boats.df$Manufacturer, "TigĂ\u0083Â© power boats", "Tige")
boats.df$Manufacturer <- str_replace_all(boats.df$Manufacturer, "KaasbĂ\u0083Â¸ll power boats", "Kaasboll")
boats.df$Manufacturer <- str_replace_all(boats.df$Manufacturer, "ManĂ\u0083Â˛ power boats", "Mano")
boats.df$Manufacturer <- str_replace_all(boats.df$Manufacturer, "SkilsĂ\u0083Â¶ power boats", "Skilso")

boats.df$Manufacturer <- str_replace_all(boats.df$Manufacturer, " power boats", "")


fuel.types <- c("Unleaded", "Diesel", "Electric", "Gas", "Hybrid", "Propane")
#
boats.df <- boats.df
boats.df$Fuel.Type <- NA
for (c in 1:nrow(boats.df))
{
  splitted <- str_split_fixed(boats.df[c,]$Type, ",", 2)
  status <- splitted[1]
  fuel.type <- splitted[2]
  if (status %in% fuel.types) {
    fuel.type <- status
    status <- NA

  }
  boats.df[c,]$Type <- tolower(status)
  boats.df[c,]$Fuel.Type <- tolower(fuel.type)

}
df <- apply(boats.df,2,as.character)
write.csv(df, "./data/boat_data_cleared.csv", row.names=FALSE)