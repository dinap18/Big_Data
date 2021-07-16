#Big Data Assignment 2- Week 5
#Dina Pinchuck 
#Odel Fhima    

#libraries 
library(stringr)
library(dplyr)
library(ggfortify)
library(ggplot2)
library(mosaic)
library(dplyr)
library(stringr)
library(xtable)
library(gridExtra)
library(stopwords)
library(quanteda)
library(parallel)
library(caret)
library(caTools)
library(e1071)
library(rpart)
library(rpart.plot)
# # Set rounding to 2 digits
 # options(digits=2)
# 
# ## ----cache=TRUE, warning=FALSE, message=FALSE----------------------------
profiles <- read.csv( file.path( 'okcupid_profiles.csv' ), header=TRUE, stringsAsFactors=FALSE)
# 
# #total number of profiles - used for trimming dfm
n <- nrow(profiles)
# 
str(profiles)
gender <- profiles$sex
# 
# ##essays 
# 
essays <- select(profiles, starts_with("essay"))
essays <- apply(essays, MARGIN = 1, FUN = paste, collapse=" ")

rm(profiles)
# # 
# # Tokenize essay texts
all.tokens <- tokens(essays, what = "word",
                      remove_numbers = TRUE, remove_punct = TRUE,
                      remove_symbols = TRUE, remove_hyphens = TRUE)


# # Lower case the tokens.
 all.tokens <- tokens_tolower(all.tokens)
# 
# # Use quanteda's built-in stopword list for English.
# # NOTE - You should always inspect stopword lists for applicability to
# #        your problem/domain.
all.tokens <- tokens_select(all.tokens, stopwords(),
                             selection = "remove")
 
 all.tokens <- tokens_wordstem(all.tokens, language = "english")
# 
# #dfm
all.tokens.dfm <- dfm(all.tokens, tolower = TRUE)
print("words before trimming")
print(head(featnames(all.tokens.dfm),100))
rm(all.tokens) #tokens are no longer needed

#transform into matrix and take off unimportant words so we can get a faster run time and better results
all.tokens.dfm<-dfm_trim(all.tokens.dfm,  min_termfreq =0.50*n,min_docfreq=2)
print("words after trimming")
print(featnames(all.tokens.dfm))
all.tokens.matrix <- as.matrix(all.tokens.dfm)


rm(all.tokens.dfm) #dfm is no longer needed
gc()
#tf - relative term frequency
term.frequnecy<-function(row){
  row/sum(row)
}
#idf- inverse document frequency
inverse.doc.freq<-function(col){
  corpus.size<-length(col)
  doc.count<-length(which(col>0))
  log10(corpus.size/doc.count)
}
tf.idf<-function(tf,idf)
{
  tf*idf
}
print("before tf-idf")
#apply tf on the rows of the matrix
tf_matrix<-apply(all.tokens.matrix,1,term.frequnecy)

print("after tf")
gc()
#apply idf on the columns of the matrix
idf_matrix<-apply(all.tokens.matrix,2,inverse.doc.freq)
print("after idf")
gc()
#apply tf-idf
train.dfm<-apply(tf_matrix,2,tf.idf,idf=idf_matrix)

print("after tf-idf")

#transpose matrix back to original form
train.dfm<-t(train.dfm)
print("after transpose on tf-idf")

#fixing incomplete cases
incomplete<-which(!complete.cases(train.dfm))
train.dfm[incomplete,]<-rep(0.0,ncol(train.dfm))

rm(tf_matrix,idf_matrix,all.tokens.matrix,incomplete)
gc()

#make a data frame
all.tokens.df <- as.data.frame(train.dfm)
#recitfy the names of the variables
names(all.tokens.df)<-make.names((names(all.tokens.df)))

#add labels of male / female to the data frame
all.tokens.df <- cbind(Label = gender,Data=all.tokens.df )

#10 fold cross validation 3 times
set.seed(123)
fitControl <- trainControl(method = "repeatedcv",
                           ## 10-fold CV...
                           number = 10,
                           ## repeated three times
                           repeats = 3
                           ,savePredictions = "final")


#running the training model
train.model<-train(all.tokens.df[,-1],all.tokens.df[,1] , method = "rpart", trControl =fitControl)

#run times
print(train.model$times)

#confusion matrix
confusion_matrix<-confusionMatrix(train.model$pred$pred,train.model$pred$obs)
print(confusion_matrix)

rm(confusion_matrix,all.tokens.df,gender)
gc()


#taking the important gender words from the decision tree
gender_words<-names(train.model$finalModel$variable.importance)
gender_words<-gender_words %>% stringr::str_remove("Data.")

print(gender_words)

pdf('Week5_datingNLP.pdf')

#tokenizing the essays
all.tokens <- tokens(essays, what = "word",
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_hyphens = TRUE)
rm(essays)
# # Lower case the tokens.
all.tokens <- tokens_tolower(all.tokens)

#removing stopwords from the tokens
all.tokens <- tokens_select(all.tokens, stopwords(),
                            selection = "remove")


all.tokens <- tokens_wordstem(all.tokens, language = "english")
#removing gender words from the tokens
all.tokens<-all.tokens %>% 
  tokens_remove(pattern = phrase(gender_words), valuetype = 'fixed')


gc()
# #dfm
all.tokens.dfm <- dfm(all.tokens, tolower = TRUE)
print("words before trimming")
print(head(featnames(all.tokens.dfm),100))
rm(all.tokens)
gc()
#transform into matrix and removing the unimportant words so we can get better results
all.tokens.dfm<-dfm_trim(all.tokens.dfm,  min_termfreq = 0.50*n,min_docfreq=2)
print("words after trimming")
print(featnames(all.tokens.dfm))
all.tokens.matrix <- as.matrix(all.tokens.dfm)


rm(all.tokens.dfm)


print("before tf-idf")
#apply tf on the rows of the matrix
tf_matrix<-apply(all.tokens.matrix,1,term.frequnecy)

print("after tf")
gc()
#apply idf on the columns of the matrix
idf_matrix<-apply(all.tokens.matrix,2,inverse.doc.freq)
print("after idf")
gc()
#apply tf-idf
train.dfm2<-apply(tf_matrix,2,tf.idf,idf=idf_matrix)

print("after tf-idf")

#transpose matrix back to original form
train.dfm2<-t(train.dfm2)
print("after transpose on tf-idf")

#fixing incomplete cases
incomplete<-which(!complete.cases(train.dfm2))
train.dfm2[incomplete,]<-rep(0.0,ncol(train.dfm2))

rm(tf_matrix,idf_matrix,all.tokens.matrix,incomplete)
gc()


#running the pca algorthim of the train dfm (result of tf-idf)
clusters.pca<-prcomp(train.dfm2)

print("before two kmeans")
#kmeans with 2 clusters + graph
two_kmeans<-kmeans(train.dfm2, 2) 
kmeansData1 <- cbind(clusters.pca$x[,1:2], Cluster=as.factor(two_kmeans$cluster))
print(ggplot() +  geom_point(data = as.data.frame( kmeansData1),aes(x=PC1,y=PC2,colour=factor(Cluster)))+scale_colour_hue()+ggtitle("2 Clusters Kmeans") + labs(color='Clusters')  )
rm(two_kmeans,kmeansData1)
gc()

print("before three kmeans")
#kmeans with 3 clusters + graph
three_kmeans<-kmeans(as.matrix(train.dfm2), 3)
kmeansData2 <- cbind(clusters.pca$x[,1:2], Cluster=as.factor(three_kmeans$cluster))
print(ggplot() +  geom_point(data = as.data.frame( kmeansData2),aes(x=PC1,y=PC2,colour=factor(Cluster)))+scale_colour_hue()+ggtitle("3 Clusters Kmeans") + labs(color='Clusters')  )


rm(three_kmeans,kmeansData2)
gc()

print("before four kmeans")
#kmeans with 4 clusters + graph
four_kmeans<-kmeans(as.matrix(train.dfm2), 4)
kmeansData3 <- cbind(clusters.pca$x[,1:2], Cluster=as.factor(four_kmeans$cluster))
print(ggplot() +  geom_point(data = as.data.frame( kmeansData3),aes(x=PC1,y=PC2,colour=factor(Cluster)))+scale_colour_hue()+ggtitle("4 Clusters Kmeans") + labs(color='Clusters')  )

rm(four_kmeans,kmeansData3)
gc()

print("before 10 kmeans")
#kmeans with 10 clusters + graph
ten_kmeans<-kmeans(as.matrix(train.dfm2), 10)
kmeansData4 <- cbind(clusters.pca$x[,1:2],Cluster= as.factor(ten_kmeans$cluster))
print(ggplot() +  geom_point(data = as.data.frame( kmeansData4),aes(x=PC1,y=PC2,colour=factor(Cluster)))+scale_colour_hue()+ggtitle("10 Clusters Kmeans") + labs(color='Clusters')  )


rm(ten_kmeans,kmeansData4)
gc()

#turning off the pdf
dev.off()


#saving data
save(file='Week5_datingNLP.rdata',train.model,train.dfm,train.dfm2, clusters.pca)


