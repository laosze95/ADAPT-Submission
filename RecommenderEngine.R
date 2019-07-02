#Install the required packager and load it in memory
install.packages("recommenderlab")
library("recommenderlab")

#Creating small dataset
m <- matrix(sample(c(as.numeric(0:5), NA), 50,
                   replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
            dimnames=list(user=paste("u", 1:5, sep=""),
                          item=paste("i", 1:10, sep="")))


#With coercion, the matrix can be easily converted into a realRatingMatrix object which stores
#the data in sparse format (only non-NA values are stored explicitly; NA values are represented by a dot).
r <- as(m, "realRatingMatrix")

#An important operation for rating matrices is to normalize the entries to, e.g., centering to
#remove rating bias by subtracting the row mean from all ratings in the row. This is can be
#easily done using normalize().
r_m <- normalize(r)

#We will use the data set Jester5k for the rest of this section. This data set comes with recommenderlab
#and contains a sample of 5000 users from the anonymous ratings data from the
#Jester Online Joke Recommender System collected between April 1999 and May 2003
data(Jester5k)
set.seed(1234)
r <- sample(Jester5k, 1000)


#Next, we create a recommender which generates recommendations solely on the popularity of items
r <- Recommender(Jester5k[1:1000], method = "POPULAR")


#Here we create top-5 recommendation lists for two users who were not used to learn the model.
recom <- predict(r, Jester5k[1001:1002], n=5)


#We can get the best 3 recommendations for each list using bestN()
recom3 <- bestN(recom, n = 3)


#Many recommender algorithms can also predict ratings. This is also implemented using
#predict() with the parameter type set to "ratings"
recom <- predict(r, Jester5k[1001:1002], type="ratings")

#Alternatively, we can also request the complete rating matrix which includes the original
#ratings by the user
recom <- predict(r, Jester5k[1001:1002], type="ratingMatrix")


#Evaluation of predicted ratings
e <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9, given=15, goodRating=5)


#We create two recommenders (user-based and item-based collaborative filtering) using the
#training data.
r1 <- Recommender(getData(e, "train"), "UBCF")


#Recommender of type 'UBCF' for 'realRatingMatrix' learned using 900 users.
r2 <- Recommender(getData(e, "train"), "IBCF")


#Next, we compute predicted ratings for the known part of the test data (15 items for each user) 
#using the two algorithms.
p1 <- predict(r1, getData(e, "known"), type="ratings")
p2 <- predict(r2, getData(e, "known"), type="ratings")


#Finally, we can calculate the error between the prediction and the unknown part of the test data.
error <- rbind(
  UBCF = calcPredictionAccuracy(p1, getData(e, "unknown")),
  IBCF = calcPredictionAccuracy(p2, getData(e, "unknown")))


#Evaluation of a top-N recommender algorithm
scheme <- evaluationScheme(Jester5k[1:1000], method="cross", k=4, given=3, goodRating=5)


#Next we use the created evaluation scheme to evaluate the recommender method popular.
#We evaluate top-1, top-3, top-5, top-10, top-15 and top-20 recommendation lists.
results <- evaluate(scheme, method="POPULAR", type = "topNList", n=c(1,3,5,10,15,20))
getConfusionMatrix(results)[[1]]
avg(results)
plot(results, annotate=TRUE)



#Comparing recommender algorithms
set.seed(2016)
scheme <- evaluationScheme(Jester5k[1:1000], method="split", train = .9,
                           k=1, given=-5, goodRating=5)


algorithms <- list(
  "random items" = list(name="RANDOM", param=NULL),
  "popular items" = list(name="POPULAR", param=NULL),
  "user-based CF" = list(name="UBCF", param=list(nn=50)),
  "item-based CF" = list(name="IBCF", param=list(k=50)),
  "SVD approximation" = list(name="SVD", param=list(k = 50)))

## run algorithms
results <- evaluate(scheme, algorithms, type = "topNList", n=c(1, 3, 5, 10, 15, 20))
plot(results, annotate=c(1,3), legend="bottomright")
plot(results, "prec/rec", annotate=3, legend="topleft")

