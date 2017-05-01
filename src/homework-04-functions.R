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

##A függvény argumentumok megadása nélkül Hillary Clinton első 5 tweetjét adja.
##A függvény errort dob, ha nem Hillary Clintont vagy Donald Trumpot írjuk be,
#kiírja a hiba okát. Szintén errort dob, ha túl nagy számot írunk be (csak akkor
#vizsgálja, ha érvényes jelöltnevet írtunk be, ekkor megvizsgálja, hogy milyen
#hosszú az a vektor, amiben azok a sorok szerepelnek, amik az adott jelölt
#twitterazonosítóját tartalmazzák.)
##Ha az argumentumok rendben vannak, akkor a függvény létrehozza a retweetek és
#favoritok összegét tartalmazó oszlopot, és hozzáragasztja a tweets df végéhez.
##Ezután subsetelek adott jelöltre úgy, hogy csak a tweet szövegét és ezt az
#összeget tartalmazó oszlopok legyenek benne, utána ezt a subsetet rendezem az
#összeg szerint csökkenő sorrendbe.
##Végül a függvény ennek a subsetnek az elején lévő tweeteket írja ki, méghozzá 
#annyi esetet, amennyi a második argumentum.

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
