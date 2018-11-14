library(twitteR)

consumerKey = ""
consumerSecret = ""
accessToken = ""
accessSecret = ""


cT.tweets = searchTwitter('exemplo',n =20)
df = do.call("rbind",lapply(cT.tweets,as.data.frame))
## TM serve para limpar os tweets 
library(tm)
df_tweets = df$text
df_Corpus = Corpus(VectorSource(df_tweets))

# Remove pontuação. O pacote já faz isso.
tweets_clean = tm_map(tweets_clean,removePunctuation)
# Converte para minúsculas
tweets_clean = tm_map(tweets_clean,content_transformer(tolower))
# Remover termos já estabelecidos
tweets_clean = tm_map(tweets_clean,removeWords,c("exemplo","exemplo2","exemplo3"))

#Começamos aqui

df_cleanedTweets <-  data.frame(text=get("content", tweets_clean), 
    stringsAsFactors=F)
list_cleanedTweets = as.list(df_cleanedTweets$text)

library(stringr)

list_cleanedTweets = lapply(list_cleanedTweets,function(x) strsplit(x,split = " "))
unlist_CleanedTweets = sapply(list_cleanedTweets,unlist)

tweetsdapuc = unlist_CleanedTweets

getwd()
palavras.positivas = scan("exemplo.txt",what = "character",comment.char = ";")
palavras.negativas = scan("exemplo2.txt",what = "character",comment.char = ";")

#Score
score.positivo = lapply(tweetsdapuc,function(x){sum(!is.na(match(x,pos.words)))})
score.negativo = lapply(tweetsdapuc,function(x){sum(!is.na(match(x,neg.words)))})

#Score neutro é só a ausência de scores positivo ou negativos
score.neutro = lapply(tweetsdapuc,function(x){sum(!is.na(match(x,pos.words)))-sum(!is.na(match(x,neg.words)))})

positivo = unlist(score.positivo) 
negativo = unlist(score.negativo) 
sentimento.neutro = unlist(score.neutro)

sentimento.neutro[sentimento.neutro>0]="Positivo"
sentimento.neutro[sentimento.neutro<0]="Negativo"
sentimento.neutro = ifelse(sentimento.neutro=="0","Neutro",sentimento.neutro)
sentimento.neutro = as.factor(sentimento.neutro)
#Porcentagem 
prop.table(table(sentimento.neutro))

