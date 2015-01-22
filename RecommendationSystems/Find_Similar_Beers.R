#### Building a recommender system using Beer Advocate Data ####

beerData <- read.csv("/Users/Tesla/Downloads/beer_reviews/beer_reviews.csv")


beerData <- beerData[,c("beer_beerid", "beer_name", "beer_style", "brewery_name", "review_profilename",
                        "review_overall", "review_aroma", "review_palate", "review_taste", "beer_abv",
                        "brewery_id", "review_time", "review_appearance")]
beerData_numReviews <- data.frame(table(beerData$beer_beerid))

names(beerData_numReviews)[1] <- "beer_beerid"

beerData_numReviews <- beerData_numReviews[which(beerData_numReviews$Freq >= 500),]

beerData <- merge(beerData, beerData_numReviews, by = "beer_beerid")

beerData <- beerData[order(beerData$beer_beerid, beerData$Freq),]

# Terms not present/used: brerwery_id, review_time, review_appearnace

## Finding Similarities ##
# The goal of the system will be for a user to provide us with a beer that they know and love, and for 
# us to recommend a new beer which they might like, To accomplish this, we are going to use 
# collaborative filtering. We are going to compare two beers by ratings submitted by their common reviewer
# Then, when one user writes similar reviews for two beers, we'll then consider those two beers
# to be more similar to one another

# We'll need a function which takes two beers and returns their mutual reviewers (or sameset). To do this
# we'll use the intersect function in R which finds common elements between two lists or vectors.

## End: Finding Similarities ##

common_reviewers_by_id <- function(beer1, beer2){
  reviews1 <- subset(beerData, beer_beerid == beer1)
  reviews2 <- subset(beerData, beer_beerid == beer2)
  reviewers_sameset <- intersect(reviews1[,"review_profilename"], reviews2[,"review_profilename"])
  if(length(reviewers_sameset) == 0){
    NA
  }else{
    reviewers_sameset
  }
}

beer_lookup <- beerData[,c("beer_beerid", "beer_name")]
beer_lookup <- beer_lookup[duplicated(beer_lookup) == FALSE,]

common_reviewers_by_name <- function(name1, name2){
  beer1 <- subset(beer_lookup, beer_name == name1)$beer_beerid
  beer2 <- subset(beer_lookup, beer_name == name2)$beer_beerid
  common_reviewers_by_id(beer1, beer2)
}

common_reviewers_by_id(34146,837)

common_reviewers_by_name("Founders Double Trouble", "Coors Light")

### Next we need a function to extract features for a given beer. Features in this case, are the 
## 1 to 5 numerical ratings provided by users as part of each beer's reviews ##
### End: Beer Review ###

features <- c("review_overall", "review_aroma", "review_palate", "review_taste")
get_review_metrics <- function(beer, userset){
  beer.data <- subset(beerData, beer_beerid == beer & review_profilename %in% userset)
  o <- order(beer.data$review_profilename)
  beer.data <- beer.data[o,]
  dups <- duplicated(beer.data$review_profilename)==FALSE
  beer.data <- beer.data[dups,]
  # This can return more than one type of metric
  beer.data[,features]
}

head(reviews)

### Quantifying our beliefs ###
calc_similarity <- function(b1, b2){
  common_users <- common_reviewers_by_id(b1, b2)
  if(is.na(common_users)){
    return (NA)
  }
  beer1.reviews <- get_review_metrics(b1, common_users)
  beer2.reviews <- get_review_metrics(b2, common_users)
  # this can be more complex; we're just taking a weighted average 
  weights <- c(2,1,1,1)
  
  corrs <- sapply(names(beer1.reviews), function(metric){
    cor(beer1.reviews[metric], beer2.reviews[metric])
  })
  
  sum(corrs * weights, na.rm = TRUE)
}

beer_name_to_id <- function(beerName){
  dataBeer <- subset(beerData, beer_name == beerName)
  return(unique(dataBeer$beer_beerid))
}
 
b1 <- beer_name_to_id("Fat Tire Amber Ale")
b2 <- beer_name_to_id("Dale's Pale Ale")
calc_similarity(b1, b2)


b2 <- beer_name_to_id("Michelob Ultra")
calc_similarity(b1, b2)

### Computing Similarity across all 2-Beer-Pairs ###
install.packages("plyr")
require("plyr")
beer.counts <- ddply(beerData, .(beer_name), nrow)
o <- order(-beer.counts$V1)
# Get the 20 most commonly reviewed beers
all.beers <- head(beer.counts[o,],20)$beer_name
beer.pairs <- expand.grid(beer1 = all.beers, beer2 = all.beers)

beer.pairs <- subset(beer.pairs, beer1 != beer2)

results <- ddply(beer.pairs, .(beer1, beer2), function(x){
  b1 <- beer_name_to_id(x$beer1)
  b2 <- beer_name_to_id(x$beer2)
  c("sim" = calc_similarity(b1,b2))
}, .progress = "text")

### Helper function find_similar_beers accepts a beer you like and optionally a number of
## suggested beers and a desired style, and returns the most similar beers in a nice format

beers <- beerData[,c(2:4)]
beers <- unique(beers)

find_similar_beers <- function(mybeer, style = NULL, n = 5){
  similar <- subset(results, beer1 == mybeer)
  similar <- merge(beers, similar, by.x = "beer_name", by.y = "beer2")
  if(!is.null(style)){
    similar <- subset(similar, beer_style == style)
  }
  similar <- similar[order(-similar$sim),]
  n <- min(n, nrow(similar))
  similar <- similar[1:n, c("brewery_name", "beer_name", "beer_style", "sim")]
  similar
}

test_results <- find_similar_beers(mybeer = "Sierra Nevada Pale Ale", style = "American IPA", n = 5)
