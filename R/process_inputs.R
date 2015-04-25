library(XML)
library(stringr)
library(dplyr)

process <- function(input_filename, output_dir, resolution_filename) {
  
  html.raw <- htmlTreeParse(input_filename, useInternalNodes=T)
  
  html.parse <- xpathApply(html.raw, "//p", xmlValue)
  
  html.text <- unlist(html.parse)
  
  html.text <- gsub('\\n', ' ', html.text)
  
  # trim text
  html.text <- str_trim(html.text)
  
  # Position of resolution section
  resolutions <- c();
  ids <- c();
  # adoption.dates <- c();
  
  res_sec_pos <- grep("^(A\\.|I\\.)\\s{1,}Resolutions", html.text);
  
  i <- res_sec_pos[length(res_sec_pos)] + 1
  while(!grepl("^(B\\.|II\\.)", html.text[[i]])) {
    
    id_matches <- str_match(html.text[[i]], "^\\d+/\\d+")
    if (!is.na(id_matches[1,1])) {
      ids <- append(ids, id_matches[1,1])
    }
    
    r_matches <- str_match_all(html.text[[i]], "^\\d+\\/\\d+(\\.)?\\s{1,}(.*)\\s{1,}\\d+$")
    r <- ''
    if (length(r_matches[[1]]) > 0) {
      r <- str_trim(r_matches[[1]][3])
      r <- gsub("\\s{2, }", " ", r)
      resolutions <- append(resolutions, r)
    } else {
      date_matches <- str_match(html.text[[i]], 
                                "\\d{1,2}\\s{1,}(January|February|March|April|May|June|July|August|September|October|November|December)\\s{1,}\\d{4}")
      r <- str_trim(html.text[[i]])
      r <- gsub("\\s{2, }", " ", r)
      if (!r %in% c("Resolution", "Title", "Date of adoption") &&
            length(strsplit(r, " ")[[1]]) > 0 &&
            is.na(id_matches[1,1]) && 
            is.na(date_matches[1,1])) {
        
        resolutions <- append(resolutions, r)
        
      }
      
    }
    i <- i + 1
  }
  
  # Remove spaces if there are more than 2 spaces between words
  html.text <- sapply(html.text, function(item) {
    gsub("\\s{2, }", " ", item)
  })
  
  # Remove special character for querying
  html.text2 <- sapply(html.text, function(item) {
    gsub("[,\\.:’']", "", item)
  })
  
  # Find matching sections
  sections <- c()
  m <- length(resolutions)
  for (i in 1:m) {
    id <- ids[i]
    res <- gsub("[,\\.:’']", "", resolutions[i])
    res <- gsub("\\(", "\\\\(", res)
    res <- gsub("\\)", "\\\\)", res)
    pattern <- paste("^(", id, "(\\.)?(\\s)?)?", res, sep="");
    
    matches <- grep(pattern, html.text2)
    
    sections <- append(sections, paste(matches, collapse = ","))
  }
  
  # df.resolutions <- data.frame(id = ids, adoption.date = adoption.dates, resolution = resolutions, section = sections)
  df.resolutions <- data.frame(id = ids, resolution = resolutions, section = sections)
  df.resolutions <- arrange(df.resolutions, section)
  df.resolutions$section <- as.character(df.resolutions$section)
  write.csv(df.resolutions, file = resolution_filename)
  
  # Extract section and write to file
  total.lines <- length(html.text)
  strip.resolutions <- gsub("[,\\.:’']", "", df.resolutions$resolution)
  for (i in 1:m) {
    print(paste("processing loop", i, ":", strip.resolutions[i]))
    id <- paste('A/HRC/', df.resolutions$id[i], sep = "")
    filename = paste(output_dir, "/", gsub("[,\\.:’'\\/\\(\\)]", "", df.resolutions$resolution[i]), sep = "");
    # Get maximum 100 character for filename
    filename = substring(filename, 0, 100)
    section <- c()
    
    froms <- df.resolutions$section[i]
    froms <- as.numeric(unlist(strsplit(froms, ",")))
    for (k in 2:length(froms)) {
      
      for(j in froms[k]:total.lines) {
        # If current line is the id of different resolution, finish loop
        matches <- regexpr("^A\\/HRC\\/\\d+\\/(\\d|L(\\.)?\\d)+$", html.text2[[j]])
        matches <- regmatches(html.text2[[j]], matches)
        if (length(matches) > 0) {
          print(paste("Next resolution reached, break loop ", i, ":", matches))
          break
        }
        # If current line end with a date, finish loop
        if (grepl("\\d{1,2} (January|February|March|April|May|June|July|August|September|October|November|December) \\d{4}$", 
                  html.text2[[j]])) {
          print(paste("break loop", i, "at section end at line", j, "th", "end with a date"))
          break
        }
        
        # If current line is the beginning of next resolution, finish loop
        r_matches <- str_match_all(html.text2[[j]], "[a-zA-Z\\s,:’'-]+")
        r <- paste(c(r_matches[[1]]), collapse = " ")
        if (r %in% strip.resolutions[strip.resolutions != strip.resolutions[i]]) {
          print(paste("Next resolution reached, break loop ", i))
          break
        }
        
        # If current line is the beginning of another section (Detecting roman number), finish loop
        if (grepl("^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})\\.", html.text[[j]])) {
          print(paste("break loop", i, "at section end at line", j, "th"))
          break
        }
        
        if (grepl("^Annex I$", html.text2[[j]])) {
          print(paste("break loop", i, "at section end at line", j, "th", ": Annex reached"))
          break
        }
        section <- append(section, html.text[[j]])
        
      } 
    }
    
    cat(section, file=filename, sep="\n")
    
  }
}