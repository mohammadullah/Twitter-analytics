library(parallel)
library(stringr)
library(qdapRegex)
library(longurl)
library(rjson)
library(xlsx)


## Function to suppress error or warnings

readUrl <- function(url, n) {
  out <- tryCatch(
    {
      if (n == 1) {
        readLines(con=url, warn=FALSE) 
      } else 
      { 
        expand_urls(url)
      }
    },
    error=function(cond) {
      
      #  return(NA)
    },
    warning=function(cond) {
      
      #  return(NULL)
    },
    finally={
      
    }
  )    
  return(out)
}


## Initialization

lncount = 0
chunk = 100

## Read File and count lines

con = file("latimesANDscience.txt", "r")
while ( TRUE ) {
  line = readLines(con, n = 1)
  if ( length(line) == 0 ) {
    break
  }
  
  lncount = lncount + 1 
}

close(con)

## Check even or odd number of lines to read files into chunk

if (lncount %% chunk == 0) {
  m = lncount/chunk
  lnmod = chunk
} else {
  lnmod <- lncount %% chunk 
  ln1 = lncount - lnmod
  m = (ln1/chunk) + 1
}

## Read lines in chunk

con1 = file("latimesANDscience.txt", "r")

for (x in 1:m) {
  
  if (x == m) {chunk = lnmod}
  
  line = readLines(con1, n = chunk)
  data1 <- line
  
  ## format lines to JESON (I do not think this is necessary, there must be other way)
  
  ## Although, the given file is in JESON format but I could not read it
  ## Beause each field is enclosed by 2-double quotation marks, for example
  ## "metadata" is written like this ""metadata"". So, I changed all 2-double 
  ## quotation to single double quotation. But I think, procedure is bit ugly, 
  ## It should be done in more elegant way.
  ## I also did some reformating at the beginning and end of each line chunk
  
  ## replace "" with some garbage  
  data1 <- gsub('\"', "pzmru", data1, fixed = TRUE)  
  
  ## replace double garbage with single quotation
  data1 <- gsub('pzmrupzmru', "\"", data1, fixed = TRUE) 
  
  ## Repace single garbage with ""
  data1 <- gsub('pzmru', "", data1, fixed = TRUE)
  
  ## Replace tab with colon
  data1 <- gsub('\t', "\":", data1, fixed = TRUE)
  
  ## Add single quotation and curly bracket at the beginning
  data1 <- paste0("{\"", data1)
  
  ## A new variabe is created excluding first and last character vector
  data2 <- data1[2:(length(data1)-1)]
  data2 <- paste0(data2, '},')
  
  ## Some formating to the first and last vector 
  first1 <- data1[1]
  last1 <- data1[length(data1)]
  first1 <- paste0(first1, '},')
  last1 <- paste0(last1, '}]')
  first1 <- paste0("[",first1)
  
  ## The final JSON format
  data_final <- c(first1, data2, last1)
  
  ## Write jeson file in the disk
  write(data_final, "latimes.json")
  
  ## read the file 
  fjson_data <- fromJSON(file = "latimes.json")
  
  ## delete objects to release memory
  rm(data2)
  rm(data_final)
  
  ## Formating done 
  
  ######################################################################################### 
  #########################################################################################
  
  ## initialize some variables
  pages <- numeric(length(fjson_data))
  id1 <- numeric(length(fjson_data))
  re_id <- numeric(length(fjson_data))
  
  
  for (i in 1:length(fjson_data)) {
    
    ##  check whether field is empty or not
    ln_check <- fjson_data[[i]][[1]]$entities$urls
    ln_check1 <- fjson_data[[i]][[1]]$retweeted_status$entities$urls
    reid_check <- fjson_data[[i]][[1]]$retweeted_status
    
    ## Read expanded_url from entity, if not available take from retweet_status
    if(length(ln_check) > 0) 
      pages[i] <- fjson_data[[i]][[1]]$entities$urls[[1]]$expanded_url
    else if(length(ln_check1) > 0)   
      pages[i] <- fjson_data[[i]][[1]]$retweeted_status$entities$urls[[1]]$expanded_url
    
    
    ## read tweet id if retweetd_status is available
    if(length(reid_check) > 0) {
      
      re_id[i] <- fjson_data[[i]][[1]]$retweeted_status$id_str
    }
  }
  
  ids <- unlist(lapply(fjson_data, names))
  
  ## Make a data frame from the first chunk of the lines
  
  if (x == 1) {
    df <- data.frame(id = ids, page = pages, 
                     re_id = re_id, stringsAsFactors = FALSE)
  }
  
  ## rbind folowing data frames 
  
  if (x > 1)
  {
    
    df2 <- data.frame(id = ids, page = pages,
                      re_id = re_id, stringsAsFactors = FALSE)
    df <- rbind(df, df2)
  }
  
  rm(fjson_data)
  
  
}

close(con1)


## Insert NA in the empty fields
df$page[df$page == 0] <- NA
df$re_id[df$re_id == 0] <- NA

## Subset only id and page and delete empty rows
df_page <- subset(df, select = c(id, page))
df_page <- df_page[!is.na(df_page$page),]

## Clean the urls from beginning and end (www.latimes----pattern)
df_page$clean1 <- str_extract(df_page$page, "www.latimes.*$")
df_page$clean1 <- str_extract(df_page$clean1, "^.*html")

## take other patterns

df_page$clean2 <- str_extract(df_page$page, 
"articles.latimes.*$|www.latimes.com/projects.*$|graphics.latimes.*$|beta.latimes.*$")

#### short urls section

## Select urls with less than 45 character
df_short <- subset(df_page, nchar(df_page$page) < 45)
## Find unique urls
short_uni <- unique(df_short$page)

## To make the conversion from short to expanded url faster 
## We use parallel processing (still very slow)

no_cores <- detectCores()
c1 <- makeCluster(no_cores)
clusterEvalQ(c1, library(longurl))
clusterExport(c1, "readUrl")
expand_list <- parLapply(c1, short_uni, function(x) readUrl(x, 2))
stopCluster(c1)

## extract expanded urls
expandedurl1 <- lapply(expand_list, "[", 2)
expandedurl1[expandedurl1 == "NULL"] <- NA
expandedurl <- unlist(lapply(expandedurl1, "[[", 1))

## create a data frame contaning short and expanded urls
df_temp <- data.frame(short_uni, expandedurl)
df_temp$short_uni <- as.character(df_temp$short_uni)
df_temp$expandedurl <- as.character(df_temp$expandedurl)

## get the urls that contain "latimes.com" pattern also nchar > 45
df_temp$clean1 <- grepl("latimes.com", df_temp$expandedurl, ignore.case = TRUE)
df_temp$clean2 <- ifelse(df_temp$clean1 == TRUE, df_temp$expandedurl, NA)
df_temp$clean <- ifelse(nchar(df_temp$clean2) > 45, df_temp$clean2, NA)

## match and place the epanded urls to the main data frame
match1 <- match(df_page$page,df_temp$short_uni)
df_page$short <- df_temp$clean[match1]

########################################################

### Twitter status/quote section

## make new data frame with quote urls
df_page$quote1 <- grepl("https://twitter.com", df_page$page)
df_quote <- subset(df_page, quote1 == TRUE, select = c(1, 2))

uni_quote <- unique(df_quote$page)
uni_quote <- as.character(uni_quote)

## Read from connection
no_cores <- detectCores()
c1 <- makeCluster(no_cores)
clusterExport(c1, "readUrl")
clusterEvalQ(c1, library(longurl))
twitter1 <- parLapply(c1, uni_quote, function(x) readUrl(x, 1))
stopCluster(c1)

### Do webscraping to extract the url

## initialize a vector to store the urls
url_1 <- numeric(length(twitter1))

for (i in 1:length(twitter1)) {
  
  test <- twitter1[[i]]
  
  ## if list is empty put some garbage in the corresponding vector
  if(length(test) < 1)
  {
    url_1[i] <- 50000
    next
  }
  
  ## find all vector cells that contain "title" in the text
  numb <- grep("title", test)
  
  ## now check the cells
  
  for (j in 1:length(numb)) {
    
    ## if the vector cell contain the expanded url then extract and exit from loop
    titles <- str_extract(test[numb[j]], "www.latimes.com(.*?)\\.html")
    if(!is.na(titles)) {
      url_1[i] <- titles
      break
    } 
    
    ## or find a particular pattern
    titles <- str_extract(test[numb[j]], "title([^<]*)\\<span")
    
    ## if empty then go to the next cell
    if(is.na(titles)) next
    
    ## or check whether the url is shorten. Expand the url, check some pattern
    ## and character length, if right pattern is found, store the url and exit the loop
    if (nchar(titles) < 45) {
      test_url <- unlist(ex_url(titles))
      test_url <- gsub("\"", "", test_url, fixed = TRUE)
      test_url_1 <- readUrl(test_url, 2)
      if(length(test_url_1) == 1) {
        next
      } else {
        
        test_url_2 <- test_url_1$expanded_url
        la_test <- grep("www.latimes.com", test_url_2)
        la_test_2 <- grep("articles.latimes.com|www.latimes.com/projects|
                          graphics.latimes|beta.latimes", test_url_2)
        
        if(length(la_test) > 0 && nchar(test_url_2) > 45) {
          url_1[i] <- test_url_2
          break
        }
        
        if(length(la_test_2) > 0 && nchar(test_url_2) > 45) {
          url_1[i] <- test_url_2
          break
        }
      }
    } else {
      
      ## if the url is not short then do the pattern check and store the url
      test_url <- unlist(ex_url(titles))
      test_url <- gsub("\"", "", test_url, fixed = TRUE)
      la_test_2 <- grep("articles.latimes.com|www.latimes.com/projects|
                        graphics.latimes|beta.latimes", test_url)
      if(length(la_test_2) > 0 && nchar(test_url) > 45) {
        url_1[i] <- test_url
        break
      }
      
    }
    }
}

## make a new data frame with quote urls and corresponding webscraped urls
## Do some cleaning
df_uni <- data.frame(quote_url = uni_quote, urls = url_1)
df_uni$urls <- unlist(ex_url(df_uni$urls))
df_uni$urls <- gsub("\"", "", df_uni$urls, fixed = TRUE)

df_uni$urls[df_uni$urls == 0] <- NA
df_uni$urls[df_uni$urls == 50000] <- NA
df_uni$quote_url <- as.character(df_uni$quote_url)

## Add the quote urls to the main data frame
match2 <- match(df_page$page, df_uni$quote_url)
df_page$quote2 <- df_uni$urls[match2]

####################################################################

## Add all cleaned columns to make the final column
## Do some final cleaning

df_page$clean <- df_page$clean1
index <- !is.na(df_page$clean2)
df_page$clean[index] <- df_page$clean2[index]
index <- !is.na(df_page$short)
df_page$clean[index] <- df_page$short[index]
index <- !is.na(df_page$quote2)
df_page$clean[index] <- df_page$quote2[index]

df_page$cl_final <- str_extract(df_page$clean, "www.latimes.*$")
df_page$cl_final <- str_extract(df_page$cl_final, "^.*html")
df_page$cl_final1 <- str_extract(df_page$clean, "articles.latimes.*$|www.latimes.com/projects.*$|graphics.latimes.*$|beta.latimes.*$")

index <- !is.na(df_page$cl_final1)
df_page$cl_final[index] <- df_page$cl_final1[index]

df_page$test <- ifelse(!is.na(df_page$clean) & is.na(df_page$cl_final), df_page$clean, NA)
df_page$test1 <- grepl("latimes.com/", df_page$test)

df_page$cl_final[df_page$test1] <- df_page$test[df_page$test1]
df_page$cl_final <- gsub("%2F", "/", df_page$cl_final, fixed = TRUE)

df_clean <- subset(df_page, select = c(1, 2, 9))
write.csv(df_clean, "latimes_final_url.csv")
