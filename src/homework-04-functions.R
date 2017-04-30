#4.) Írj egy függvényt, ami a "Hillary Clinton" bemenet esetében Hillary Clinton,
#"Donald Trump" bemenet esetében pedig Donald Trump tweetjeit adja ki a 
#retweet_count és a favorite_count összesített értéke alapján csökkenő 
#sorrendben. Legyen egy olyan argumentum is, amivel a kiadott tweetek számát 
#lehet meghatározni. Az argumentumoknak legyen default értéke. A függvény írjon 
#ki hibaüzenetet, ha rossz nevet kap. Ha a kiadott tweetek száma meghaladja a 
#lehetséges tweetek számát, írja ki a tweetek max számát. Mikor a függvény 
#kiadja a tweeteket, írja ki, hogy kinek a tweetjeit írja ki és mennyit. Kérd le
#a függvénnyel Hillary Clinton első 10, valamint Donald Trump első 15 legtöbbet 
#retweetelt és kedvelt tweetjét.

twitter.campaign <- function(candidate = "Hillary Clinton", twno = 5) {
  if (candidate != "Hillary Clinton" & candidate != "Donald Trump") {
    stop("Invalid candidate name (accepted: Hillary Clinton or Donald Trump)")
  }
  if (candidate == "Hillary Clinton" 
      & twno > length(which(tweets$handle== "HillaryClinton"))) {
    stop(paste0("Invalid tweet number, the maximum number of tweets for this
                candidate is ", length(which(tweets$handle=="HillaryClinton"))))
  }
  if (candidate == "Donald Trump" 
      & twno>length(which(tweets$handle== "realDonaldTrump"))) {
    stop(paste0("Invalid tweet number, the maximum number of tweets for this
                candidate is ", length(which(tweets$handle=="realDonaldTrump"))))
  }
  
  retfavsum <- cbind(tweets$retweet_count, tweets$favorite_count)
  retfavsum <- apply(retfavsum, 1, sum)
  tweets <- cbind(tweets, retfavsum)
  
  if (candidate == "Hillary Clinton") {
    df <- tweets[which(tweets$handle== "HillaryClinton"), c(3,35)]
    df <- df[order(df$retfavsum, na.last = T, decreasing = T), ]
  }
  
  
  if (candidate == "Donald Trump") {
    df <- tweets[which(tweets$handle == "realDonaldTrump"), c(3,35)]
    df <- df[order(df$retfavsum, na.last = T, decreasing = T), ]
  }
  return(head(df$text, n = twno))
}
