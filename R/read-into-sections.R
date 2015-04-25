library(XML)
library(stringr)
library(dplyr)

html.raw <- htmlTreeParse("./data/docs/A_HRC_25_2_session_report_AUV_for_web.htm", useInternalNodes=T)

html.parse <- xpathApply(html.raw, "//p", xmlValue)

html.text <- unlist(html.parse)

html.text <- gsub('\\n', ' ', html.text)

# trim text
html.text <- str_trim(html.text)

# Remove spaces if there are more than 2 spaces between words
html.text <- sapply(html.text, function(item) {
  gsub("\\s{2, }", " ", item)
})

# Position of resolution section
# res_sec_pos <- grep("^(A\\.|I\\.) Resolutions$", html.text);
# i <- res_sec_pos + 1
# 
# resolutions <- c();
# ids <- c();ext
# adoption.dates <- c();
# 
# while(grepl("^(B\\.|II\\.)", html.text[i])) {
#   
#   id_matches <- str_match(html.text[i], "^\\d+/\\d+")
#   if (!is.na(id_matches[1,1])) {
#     ids <- append(ids, id_matches[1,1])
#   }
#   
#   r_matches <- str_match_all(html.text[i], "[a-zA-Z\\s,:]+")
#   if (length(length(r_matches[[1]]))) {
#     r <- paste(c(resolution_matches[[1]]), sep = " ")
#     resolutions <- append(resolutions, r);    
#   }
# 
#   
# }

# Get the resolution list with their ids and adoption dates
resolutions <- c();
ids <- c();
adoption.dates <- c();

# Hard coded for position from 111 to 223, wanna improve with better way
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

write.csv(resolutions, file="./data/resolutions.csv")


# Extract section and write to file
total.lines <- length(html.text)
for (i in 1:m) {
  filename = paste("./data/drafts/", gsub("[,.:’']", "", resolutions$resolution[i]), sep="");
  section <- c()
  from <- resolutions$section[i]
  for(j in from:total.lines) {
    ## No way to detect the end of the last resolution!!!. So take next 50 lines
    if (i == m) {
      z <- j + 50
      section <- append(section, html.text[j:z])
      break
    }
    # If current line is the beginning of next resolution, finish loop
    if (gsub("[,.:’']", "", resolutions$resolution[i + 1]) == gsub("[,.:’']", "", html.text[[j]])) {
      print(paste("break loop", i, "at beginning of next resolution at line", j, "th"))
      break  
    }
    
    # If current line is the beginning of another section (Detecting roman number), finish loop
    if (grepl("^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})\\.", html.text[[j]])) {
      print(paste("break loop", i, "at section end at line", j, "th"))
      break
    }
    
    section <- append(section, html.text[[j]])
    
  }

  cat(section, file=filename, sep="\n")
  
}

