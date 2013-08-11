library(XML)
library(rjson)
library(RCurl)

### Settings

key <- '' ## put your API key for rotten tomatoes
budget_url <- "http://www.the-numbers.com/movies/records/allbudgets.php"
fromDate <- "2011-01-01"

############################################################################################
### Helpers   
############################################################################################

rt_movieSearch <- function (movie = "", apikey = "", pageLimit = 10) {
  q <- paste("http://api.rottentomatoes.com/api/public/v1.0/movies.json?apikey=", apikey, '&q=', gsub(" ", "+", movie), '&page_limit=', pageLimit, sep = "")
  fromJSON(getURL(q)) 
}

rt_movieInfo <- function(id, apikey = "") {
  q <- paste("http://api.rottentomatoes.com/api/public/v1.0/movies/", id, ".json?apikey=", apikey, sep = "")
  fromJSON(getURL(q))
}

############################################################################################
###    Get movies' budget from the numbers.com
############################################################################################

tables <- readHTMLTable(budget_url, stringsAsFactors = FALSE)
# str(tables); sapply(tables, nrow)
# manually pick the table with budgets
table <- tables[[5]]

# change the type of the different columns and drop empty ones
coln <- names(table)
table <- table[,!(names(table) %in% "Movie")]
colnames(table) <- coln[-length(coln)]
dates <- table[,1]
table[,1] <- as.Date(dates,"%m/%d/%Y")

reformatUSD <- function(x) as.numeric(gsub("(\\$|,)", "", ifelse(x == "Unknown", NA, x)))
table[,4] <- reformatUSD(table[,4])
table[,5] <- reformatUSD(table[,5])
table[,6] <- reformatUSD(table[,6])

# Take only movie after 2011
tb <- table[table[,1] >= as.Date(fromDate) & !is.na(table$`Worldwide Gross`) & !is.na(table$Budget),]

# ugly hack to rename some mispelled movies
renameMovie <- structure(c("Harry Potter and the Deathly Hallows: Part I", "Doctor Suess' The Lorax", "Mr. Poppers's Penguins", 
    "Un Prophète", "Haevnen", "Le nom des gens", "The Adventures of Tintin: Secret of the Unicorn", "Joheunnom nabbeunnom isanghannom"), 
  names = c("Harry Potter and the Deathly Hallows: Part 1", "Dr Seuss' The Lorax", 
    "Mr. Popper's Penguins", "Un prophete", "Hævnen", "The Names of Love", "The Adventures of Tintin", "The Good, the Bad, and the Weird"))
idrn <- match(tb[,2], renameMovie)
tb[!is.na(idrn),2] <- names(renameMovie)[as.vector(na.exclude(idrn))]

############################################################################################
###    Get Rotten Tomoates' ratings
############################################################################################

# get the RT movies id
rt <- sapply(1:nrow(tb), function(i) {
  # pause every 10 query to not exceed RT limit of 10 queries per second
  if (i %% 10 == 0) Sys.sleep(1)
  movie <- tb[i, 2]
  
  r <- rt_movieSearch(movie, key)
  if(r[[1]] == "Gateway Timeout") stop("API error!")
    
  # if no match, reformat movie name dropping colon, dots and accents
  if(length(r$movies) == 0) {
    mm <- iconv(gsub("(\\d+|:|\\.|,)", "", movie), to="ASCII//TRANSLIT")
    r <- rt_movieSearch(mm, key)
  }
  if(length(r$movies) == 0) {
    ii <- NULL    
    warning("\n", movie, " and " , mm, " had no match!", "\n\n")
  } else {
      ii <- unlist(sapply(1:length(r$movies), function(j) {
        if(r$movies[[j]]$year == format(tb[i,1], "%Y")) {
          j
        } else {
          if(!is.null(r$movies[[j]]$release_dates$theater)) {
            if(as.numeric(format(as.Date(r$movies[[j]]$release_dates$theater), "%Y")) >= as.numeric(format(tb[i,1], "%Y")) -1 && 
                as.numeric(format(as.Date(r$movies[[j]]$release_dates$theater), "%Y")) <= as.numeric(format(tb[i,1], "%Y")) + 1) j 
          }
        }
      }))
      if(length(ii) > 1) {
        cat("\n", movie, " matched to:")
        sapply(ii, function(iii) cat("\n\t", r$movies[[iii]]$title))
        ii <- ii[1]
      }      
    } 
  if(!is.null(ii)) r$movies[[ii]]$id else NA
})
tb <- cbind(tb, rtId = as.numeric(as.character(rt)))
tb <- tb[!is.na(tb$rtId),]

#get RT ratings and additional informations
rt <- lapply(1:nrow(tb), function(i) {
  if (i %% 10 == 0) Sys.sleep(1)

  r <- rt_movieInfo(tb[i, 'rtId'], key)
  c(title = r$title, year = r$year, critics = r$ratings$critics_score, audience = r$ratings$audience_score, thumbnail = r$posters$thumbnail,
    link = r$links$alternate, rating = r$ratings$critics_rating, genre = r$genre[1], genres = paste(r$genre, collapse = ", "))
})

tb <- cbind(tb, data.frame(do.call(rbind, rt), stringsAsFactors = FALSE))
tb$critics <- as.numeric(as.character(tb$critics))
tb$audience <- as.numeric(as.character(tb$audience))
tb <- tb[which(tb$critics > 0),]


############################################################################################
###    Plot
############################################################################################
require(rCharts)
require(data.table)

# add a linebreak between each genre
genres <- strsplit(tb$genres, ",")
genres <- sapply(genres, function(g) gsub("^ ", "", g))
tb$genres  <- sapply(genres, function(g) paste(g, collapse="<br>&emsp;"))

tb$profitability <- (tb$`Worldwide Gross` / tb$Budget) * 100

movieTable <- data.table(
  x = as.numeric(jitter(tb$critics)),
  y = log10(jitter(tb$profitability)),
  z = tb$Budget ,
    name = sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th colspan='3'>%1$s</th></tr><tr><td><img src='%2$s' height='99' width='65'></td><td align='left'>Profitability: %3$s<br>Critics score: %4$s<br>Audience score: %5$s<br>Budget: %6$s<br>Worldwide gross: %7$s<br>Genres: %8$s</td></tr></table>", 
    tb$title,
    tb$thumbnail,
    paste(format(tb$profitability, digits = 1, scientific = FALSE), "%"),
    tb$critics,
    tb$audience,
    paste("$", (tb$Budget / 10 ^ 6), " million", sep=""),
    paste("$", format(tb$`Worldwide Gross`, big.mark="'"), sep=""),
    tb$genres),
  url = tb$link,
  category = tb$rating,
  budget = tb$Budget  
)

# Split the list into rotten tomatoes categories
movieSeries <- lapply(split(movieTable, movieTable$category), function(x) {
    res <- lapply(split(x, rownames(x)), as.list)
    names(res) <- NULL
    return(res)
})

# Create the chart object
a <- rCharts::Highcharts$new()
invisible(sapply(movieSeries, function(x) a$series(data = x, type = c("bubble"), name = x[[1]]$category)))

a$plotOptions(
  bubble = list(
    cursor = "pointer", 
    minSize = 4,
    maxSize = 30,
    point = list(
      events = list(
        click = "#! function() { window.open(this.options.url); } !#"))
  )
)

a$xAxis(title = list(text = "Rotten Tomatoes Critics Score"), labels = list(format = "{value}%"))
a$yAxis(title = list(text = "log10 % profitability"), labels = list(format = "{value}"))
a$tooltip(useHTML = T, formatter = "#! function() { return this.point.name; } !#")

a$legend(title = list(text = "Select group(s) of movies"))

a$title(text = "Films since 2011: rating vs profitability")
a$subtitle(text = "The 'profitability' is the % of budget recovered. Circle size is proportional to film budget")

a$credits(text = "Source: budgets & profits from www.the-numbers.com, ratings score from Rotten Tomatoes API")

a$params$width = 595
a$params$height = 500

# Plot it!
a
a$publish('Films since 2011: rating vs profitability')
