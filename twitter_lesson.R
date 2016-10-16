# This is a comment. Hi GW! 
print("GW Data")
txt<-"Follow @GW_Data!"
print(txt)
x<-2+2
print(x)
paste(txt, x)
# A package is like an app- it's written to extend the funcitonality of R into something new. 
# Sort of like how on its own, your Iphone won't let write tweets, but with the twitter app, you can write tweets.
require(devtools)
install.packages("devtools")
devtools::install_github("mkearney/rtweet")
# Great, the packages are now installed, but they still must be required in order to be present in our workspace
require(rtweet)
require(httpuv)
twitter_token <- create_token(app = "app", # whatever you named your app
                              consumer_key = "redacted",
                              consumer_secret = "redacted") 
# Unless you want hackers, please do not make this human readable.
# Let's run a search. Let's see what people are saying on twitter about our school. 
dat<-search_tweets("#gwu", n=1500, token=twitter_token)
# Hmmm- what's in this "dat" thingy?
class(dat)
# If we have a data frame, we can use a "$" to isolate specific columns
dat$screen_name
dat$text
# We want to inspect what each tweets actually says, but twitter text is very difficult for machines to read and analyze.
# ok, let's a function to clean our tweets. 
# Sort of long, but essentially, what is does is convert the very messy text of twitter into individual, lowercase words, as well as remove the most common stopwords
tweet_cleaner<-function (tweets, min = 0, stopwords = NULL, exclude_words = NULL) 
{
  if (is.data.frame(tweets)) {
    if ("text" %in% names(tweets)) {
      tweets <- tweets$text
    }
    else {
      stop("Must supply character vector of tweets.", call. = FALSE)
    }
  }
  tweets <- tweets[!is.na(tweets)]
  tweets <- gsub("(RT|via)", "", tweets)
  tweets <- gsub("@\\w+", "", tweets)
  tweets <- gsub("\\w+'\\w+", "", tweets)
  tweets <- gsub("[[:punct:]]", "", tweets)
  tweets <- gsub("htt\\w+", "", tweets)
  tweets <- gsub("\\n", "", tweets)
  tweets <- gsub("[[:digit:]]", "", tweets)
  tweets <- gsub("[^[:alnum:] ]", "", tweets)
  tweets <- tolower(tweets)
  tweets <- lapply(tweets, function(x) unlist(strsplit(trimws(x), 
                                                       split = " ")))
  if (is.null(stopwords)) {
    stopwords <- c("a", "a's", "able", "about", "above", 
                   "according", "accordingly", "across", "actually", 
                   "after", "afterwards", "again", "against", "ain't", 
                   "all", "allow", "allows", "im", "almost", "alone", 
                   "along", "already", "also", "although", "always", 
                   "am", "among", "amongst", "an", "and", "another", 
                   "any", "anybody", "anyhow", "anyone", "anything", 
                   "anyway", "anyways", "anywhere", "amp", "apart", 
                   "appear", "appreciate", "appropriate", "are", "aren't", 
                   "around", "as", "aside", "ask", "asking", "associated", 
                   "at", "available", "away", "awfully", "b", "be", 
                   "became", "because", "become", "becomes", "becoming", 
                   "been", "before", "beforehand", "behind", "being", 
                   "believe", "below", "beside", "besides", "best", 
                   "better", "between", "beyond", "both", "brief", "but", 
                   "by", "c", "c'mon", "c's", "came", "can", "can't", 
                   "cannot", "cant", "cause", "causes", "certain", "certainly", 
                   "changes", "clearly", "co", "com", "come", "comes", 
                   "concerning", "consequently", "consider", "considering", 
                   "contain", "containing", "contains", "corresponding", 
                   "could", "couldn't", "course", "currently", "d", 
                   "definitely", "described", "despite", "did", "didn't", 
                   "different", "do", "does", "doesn't", "doing", "don't", 
                   "done", "down", "downwards", "during", "e", "each", 
                   "edu", "eg", "eight", "either", "else", "elsewhere", 
                   "enough", "entirely", "especially", "et", "etc", 
                   "even", "ever", "every", "everybody", "everyone", 
                   "everything", "everywhere", "ex", "exactly", "example", 
                   "except", "f", "far", "few", "fifth", "first", "five", 
                   "followed", "following", "follows", "for", "former", 
                   "formerly", "forth", "four", "from", "further", "furthermore", 
                   "g", "get", "gets", "getting", "given", "gives", 
                   "go", "guys", "goes", "going", "gone", "got", "gotten", 
                   "greetings", "guy", "h", "had", "hadn't", "happens", 
                   "hardly", "has", "hasn't", "have", "haven't", "having", 
                   "he", "he's", "hello", "help", "hence", "her", "here", 
                   "here's", "hereafter", "hereby", "herein", "hereupon", 
                   "hers", "herself", "hi", "him", "hey", "himself", 
                   "his", "hither", "hopefully", "how", "howbeit", "however", 
                   "i", "i'd", "i'll", "i'm", "i've", "ie", "if", "ignored", 
                   "immediate", "in", "inasmuch", "inc", "indeed", "indicate", 
                   "indicated", "indicates", "inner", "insofar", "instead", 
                   "into", "inward", "is", "isn't", "it", "it'd", "it'll", 
                   "it's", "its", "itself", "j", "just", "k", "keep", 
                   "keeps", "kept", "know", "knows", "known", "l", "last", 
                   "lately", "later", "latter", "latterly", "least", 
                   "less", "lest", "let", "let's", "like", "liked", 
                   "likely", "little", "look", "looking", "looks", "ltd", 
                   "m", "mainly", "many", "may", "maybe", "me", "mean", 
                   "meanwhile", "merely", "might", "more", "moreover", 
                   "most", "mostly", "much", "must", "my", "myself", 
                   "n", "name", "namely", "nd", "near", "nearly", "necessary", 
                   "need", "needs", "neither", "never", "nevertheless", 
                   "new", "next", "nine", "no", "nobody", "non", "none", 
                   "noone", "nor", "normally", "not", "nothing", "novel", 
                   "now", "nowhere", "o", "obviously", "of", "off", 
                   "often", "oh", "ok", "okay", "old", "on", "once", 
                   "one", "ones", "only", "onto", "or", "other", "others", 
                   "otherwise", "ought", "our", "ours", "ourselves", 
                   "out", "outside", "over", "overall", "own", "p", 
                   "particular", "particularly", "per", "perhaps", "placed", 
                   "please", "plus", "possible", "presumably", "probably", 
                   "provides", "q", "que", "quite", "qv", "r", "rather", 
                   "rd", "re", "they'd", "they'll", "they're", "they've", 
                   "try", "trying", "twice", "two", "u", "un", "under", 
                   "unfortunately", "unless", "unlikely", "until", "unto", 
                   "up", "upon", "us", "use", "used", "useful", "uses", 
                   "using", "usually", "uucp", "v", "value", "various", 
                   "very", "via", "viz", "vs", "w", "want", "wants", 
                   "was", "wasn't", "way", "we", "we'd", "we'll", "we're", 
                   "we've", "welcome", "well", "went", "were", "weren't", 
                   "what", "what's", "whatever", "when", "whence", "whenever", 
                   "where", "where's", "whereafter", "whereas", "whereby", 
                   "wherein", "whereupon", "wherever", "whether", "which", 
                   "while", "whither", "who", "who's", "whoever", "whole", 
                   "whom", "whose", "why", "will", "willing", "wish", 
                   "with", "within", "without", "won't", "wonder", "would", 
                   "would", "wouldn't", "x", "y", "yes", "yet", "you", 
                   "you'd", "you'll", "you're", "you've", "your", "yours", 
                   "yourself", "yourselves", "z", "zero", "really", 
                   "reasonably", "regarding", "regardless", "regards", 
                   "relatively", "respectively", "right", "s", "said", 
                   "same", "saw", "say", "saying", "says", "second", 
                   "secondly", "see", "seeing", "seem", "seemed", "seeming", 
                   "seems", "seen", "self", "selves", "sensible", "sent", 
                   "serious", "seriously", "seven", "several", "shall", 
                   "she", "should", "shouldn't", "since", "six", "so", 
                   "some", "somebody", "somehow", "someone", "something", 
                   "sometime", "sometimes", "somewhat", "somewhere", 
                   "soon", "sorry", "specified", "specify", "specifying", 
                   "still", "sub", "such", "sup", "sure", "t", "t's", 
                   "take", "taken", "tell", "tends", "th", "than", "thank", 
                   "thanks", "thanx", "that", "that's", "thats", "the", 
                   "their", "theirs", "them", "themselves", "then", 
                   "thence", "there", "there's", "thereafter", "thereby", 
                   "therefore", "therein", "theres", "thereupon", "these", 
                   "they", "think", "third", "this", "thorough", "thoroughly", 
                   "those", "though", "three", "through", "throughout", 
                   "thru", "thus", "to", "together", "too", "took", 
                   "toward", "towards", "tried", "tries", "truly")
  }
  tweets <- lapply(tweets, function(x) unlist(sapply(unlist(x), 
                                                     function(y) y[!y %in% stopwords], USE.NAMES = FALSE)))
  tweets <- lapply(tweets, function(x) unlist(sapply(unlist(x), 
                                                     function(y) y[!y %in% exclude_words], USE.NAMES = FALSE)))
  tweets <- lapply(tweets, function(x) unlist(sapply(unlist(x), 
                                                     function(y) y[y != ""], USE.NAMES = FALSE)))
  freq <- table(unlist(tweets))
  tweets <- lapply(tweets, function(x) {
    x[x %in% names(freq)[freq >= min]]
  })
  tweets
}

# We can also create a new column by $ operator. 

dat$words<-tweet_cleaner(dat)

# What does this look like now?

dat$words

# OK, let's visualize this. 

# install.packages("wordcloud")
# install.packages(tm)
require(tm)
require(wordcloud)
words<-Corpus(VectorSource(dat$words))
wordcloud(words)

# Next, let's score them according to sentiment. Put everything from my github into your working directory!
# Change your file path accordingly.
pos.words<-read.table('~/GW Twitter Lession/positive-words.txt')
colnames(pos.words)<-c("words")
neg.words<-read.table('~/GW Twitter Lession//negative-words.txt')
colnames(neg.words)<-c("words")

# reading in positive and negative lexicons


sentiment_score<-function(dat)
{
  
  dat$sentiment<-rep(0, nrow(dat))
  for (i in 1:nrow(dat)){
  
  dat$sentiment[[i]]<-sum(dat$words[[i]] %in% pos.words$words)-sum(dat$words[[i]] %in% neg.words$words)
  }
  return(dat$sentiment)
}

dat$sentiment<-sentiment_score(dat$sentiment)

plot(density(dat$sentiment))

# this is really simple. Sentiment=the sum of matches in positive words- the sum of matches in negative words. 

plot(density(dat$sentiment))
mean(dat$sentiment)
sd(dat$sentiment)
words<-Corpus(VectorSource(dat[dat$sentiment==0,]$words))
wordcloud(words)
words<-Corpus(VectorSource(dat[dat$sentiment<0,]$words))
wordcloud(words)
words<-Corpus(VectorSource(dat[dat$sentiment>0,]$words))
wordcloud(words)
# Ok, now let's create a bot. In this case, I'm going to use the @GW_data account to reply to any tweets with the hashtag #gwdata2016
# The reply will be dependant on the sentiment of the tweet though!

post_tweet(status="Hi Everyone!", token=twitter_token)
dat<-search_tweets("#gwdata2016", token=twitter_token)
dat$words<-tweet_cleaner(dat)
dat$sentiment<-sentiment_score(dat)
reply_tweet<-function(dat)
  {
  
  if (dat$sentiment==0){
    tweet_text<-paste0("Glad to see @",dat$screen_name ," tweeting about GW data!")
  }else if(dat$sentiment<0){
    tweet_text<-paste0("I hope that data can improve your day @",dat$screen_name ," -come to GW Data!")
  }else if(dat$sentiment>0){
    tweet_text<-paste("@",dat$screen_name, "shows that using data makes you happy!")
  }
  
  post_tweet(tweet_text, token=twitter_token)
}

# paste0 is paste, but without any spaces. Useful to concatenate strings, as well as SQL queries. 

# apply is the better way of doing a for loop. Apply takes a function, described above, and does it to every row of the data frame
  apply(dat, 1, reply_tweet)


# Obviously there are much more intersting uses for a twitter bot which can determine responses due to sentiment-
# For example, if you run a business, and you want a bot to reply to customer concerns and complaints, this is a template
# Or a spam bot. Please don't though. Thank you all so much for coming to join us tonight!

