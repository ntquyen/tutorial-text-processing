# Loading data from file, database, web

lines <- readLines('data/raw.dat')

library(stringr)

countries = 'Algeria, Argentina, Austria, Benin, Botswana, Brazil, Burkina Faso, Chile, China, Congo, Costa Rica, Côte d’Ivoire, Cuba, Czech Republic, Estonia, Ethiopia, France, Gabon, Germany, India, Indonesia, Ireland, Italy, Japan, Kazakhstan, Kenya, Kuwait, Maldives, Mexico, Montenegro, Morocco, Namibia, Pakistan, Peru, Philippines, Republic of Korea, Romania, Russian Federation, Saudi Arabia, Sierra Leone, South Africa, the former Yugoslav Republic of Macedonia, United Arab Emirates, United Kingdom of Great Britain and Northern Ireland, Venezuela (Bolivarian Republic of), Viet Nam, United States of America'
countries.vector = str_trim(str_split(countries, ",")[[1]])

# Convert each line to element of votes list
votes.list <- mlply(lines, function(line){
  # split each line to two sentences
  parts <- str_split(line, "\\.")
  # parse first sentence
  first <- parts[[1]][1]
  first_parts <- str_split(first, ",")
  request_country = str_split(first_parts[[1]][2], "of the ")[[1]][2]
  # parse second sentence
  second <- parts[[1]][2]
  second_parts = str_split(second, "In favour: ")
  favour_part = str_split(second_parts[[1]][2], "Against: ")
  favours = str_trim(str_split(favour_part[[1]][1], ",")[[1]])
  against_part = str_split(favour_part[[1]][2], "Abstaining: ")
  againsts = str_trim(str_split(against_part[[1]][1], ",")[[1]])
  abstains = str_trim(str_split(against_part[[1]][2], ",")[[1]])
  list(favours=favours, againsts=againsts, abstains=abstains)
})


# create data frame from list of votes
votes.dataframe = ldply(votes.list, function(vote){
  row = c()
  if(length(vote$favours) > 0){
    row[vote$favours] = 'FAVOUR'
  }
  if(length(vote$againsts) > 0){
    row[vote$againsts] = 'AGAINST'
  }
  if(length(vote$abstains) > 0){
    row[vote$abstains] = 'ABSTAIN'  
  }
  row[countries.vector]
})

print(votes.dataframe)
write.csv(votes.dataframe, "data/votes.csv")