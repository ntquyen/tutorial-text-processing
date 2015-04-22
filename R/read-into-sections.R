# setwd("~/Documents/resources/Courses/projects/ir/tutorial-text-processing")

library(XML)
library(stringr)
library(dplyr)

html.raw <- htmlTreeParse("./data/A_HRC_25_2_session_report_AUV_for_web.htm", useInternalNodes=T)

html.parse <- xpathApply(html.raw, "//p", xmlValue)

html.text <- unlist(html.parse)

html.text <- gsub('\\n', ' ', html.text)

# trim text
html.text <- str_trim(html.text)

# Remove spaces if there are more than 2 spaces between words
html.text <- sapply(html.text, function(item) {
  gsub("\\s{2, }", " ", item)
})

# Get the resolution list with their ids and adoption dates
resolutions <- c();
ids <- c();
adoption.dates <- c();
for (i in seq(111, 223, by = 3)) {
  resolutions <- append(resolutions, html.text[[i]])
  ids <- append(ids, html.text[[i - 1]])
  adoption.dates <- append(adoption.dates, html.text[[i + 1]])
}

# Remove special character for querying
html.text2 <- sapply(html.text, function(item) {
  gsub("[,.:’']", "", item)
})

# Find matching sections
sections <- c()
m <- length(resolutions)
for (i in 1:m) {
  res <- gsub("[,.:’']", "", resolutions[i])
  
  matches <- grep(paste("^", res, sep=""), html.text2)
  
  sections <- append(sections, matches[2])
}

resolutions <- data.frame(id = ids, adoption.date = adoption.dates, resolution = resolutions, section = sections)
resolutions <- arrange(resolutions, section)

write.csv(resolutions, file="resolutions.csv")

# Extract section and write to file
for (i in 1:m) {
  filename = paste("./data/", gsub("[,.:’']", "", resolutions$resolution[i]), 
                   ".txt", sep="");
  from <- resolutions$section[i]
  lines <- 50
  
  # A section is rerely larger than 50 lines
  if (i < m && resolutions$section[i + 1] - from < lines) {
    lines <- resolutions$section[i + 1] - 1
  }
  to <- from + lines
  cat(html.text[from:to], file=filename, sep="\n")
  
}

View(resolutions)
View(html.text)


