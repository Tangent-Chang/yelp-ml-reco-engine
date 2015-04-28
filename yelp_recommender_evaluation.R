# http://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf
# http://datasciencelondon.org/recommender-systems-in-r-by-tamas-jambor-sky/

#install.packages("recommenderlab")
#install.packages("plyr")

# construct a matrix for users and businesses
library(recommenderlab)
library(plyr)
library(flux)

# read the reviews
yelp_review = read.csv("Desktop/Recommender/yelp_academic_dataset_review_peoria_no_text.csv", stringsAsFactors=FALSE)

# project relevant columns
yelp_rating_flat = yelp_review[,c("user_id","business_id","stars")]
# discard non-unique entries (based on user_id and business_id only)
yelp_rating_unique = yelp_rating_flat[!duplicated(yelp_rating_flat[1:2]),]

# select reviews of only the most reviewed businesses
# topNReviewedBusinesses = 247
# business_review_frequency = data.frame(table(yelp_rating_unique$business_id))
# top_n_most_reviewed_businesses = business_review_frequency[order(-business_review_frequency$Freq),][1:topNReviewedBusinesses,]
# yelp_rating_unique = yelp_rating_unique[yelp_rating_unique$business_id %in% top_n_most_reviewed_businesses[,1], ]

# filter users who have made less than x reviews
#TODO: how does min user review affect evaluation?
# change minUserReviews from 10 to 3, since dataset is smaller now.
minUserReviews = 10
user_ids <- table(yelp_rating_unique$user_id)
yelp_rating_unique = yelp_rating_unique[yelp_rating_unique$user_id %in% names(user_ids)[user_ids >= minUserReviews], ]

# reshape data frame
yelp_rating = daply(yelp_rating_unique, .(user_id, business_id), function(x) x$stars)
# convert to a matrix
m = data.matrix(yelp_rating)
# create a "rating matrix"
r = as(m, "realRatingMatrix")

# clean up temp vars
# rm(yelp_rating_flat)
# rm(yelp_rating_unique)
# rm(yelp_rating)
# rm(m)
# this is a good place to save your workspace!!

var_nn = 50
var_minRating = 3
algorithms = list(
	"random" = list(name="RANDOM", param=NULL),
	"popular" = list(name="POPULAR", param=NULL),
	"ubcf-c" = list(name="UBCF", param=list(method="Cosine", nn=var_nn, minRating=var_minRating)),
	"ubcf-p" = list(name="UBCF", param=list(method="Pearson", nn=var_nn, minRating=var_minRating)),
	"ubcf-j" = list(name="UBCF", param=list(method="Jaccard", nn=var_nn, minRating=var_minRating)),
	"ibcf-c" = list(name="IBCF", param=list(method="Cosine", minRating=var_minRating)),
	"ibcf-p" = list(name="IBCF", param=list(method="Pearson", minRating=var_minRating)),
	"ibcf-j" = list(name="IBCF", param=list(method="Jaccard", minRating=var_minRating)),
	# ibcf's default is center, no need to assign, or we can try "Z-score"
	# "ubcf-c-centered" = list(name="UBCF", param=list(method="Cosine", normalize="center", nn=var_nn)),
	# "ubcf-p-centered" = list(name="UBCF", param=list(method="Pearson", normalize="center", nn=var_nn)),
	# "ubcf-j-centered" = list(name="UBCF", param=list(method="Jaccard", normalize="center", nn=var_nn)),
	# ibcf's default is center, no need to assign, or we can try "Z-score"
	# "ibcf-c-centered" = list(name="IBCF", param=list(method="Cosine", normalize="center")),
	# "ibcf-p-centered" = list(name="IBCF", param=list(method="Pearson", normalize="center")),
	# "ibcf-j-centered" = list(name="IBCF", param=list(method="Jaccard", normalize="center")),
	"svd" = list(name="SVD", param=list(categories = 20, method="Cosine", normalize = "center", normalize_sim_matrix = FALSE, alpha = 0.5, treat_na = "median", minRating = NA))
	#"pca" = list(name="PCA", param=list(categories = 20, method="Cosine", normalize = "center", normalize_sim_matrix = FALSE, alpha = 0.5, na_as_zero = FALSE, minRating = NA))
)
# IBCF doesn't provide nn as param
# PCA comes an error:'princomp' can only be used with more units than variables

# TODO: Variety of Similarity methods
# TODO: Variety of Normalization methods (centering?)

# evaluation schemes
# k-fold
kFold = evaluationScheme(r, method="cross-validation", k=10, given=minUserReviews, goodRating=4)
# change goodRating from 5 to 4
kFold_evals = evaluate(kFold, algorithms, n=c(1, 3, 5, 10, 15, 20))
# TODO: bootstrap sampling

# evaluation results
CFM_random = getConfusionMatrix(kFold_evals[["random"]])
CFM_popular = getConfusionMatrix(kFold_evals[["popular"]])
CFM_ubcf_c = getConfusionMatrix(kFold_evals[["ubcf-c"]])
CFM_ubcf_p = getConfusionMatrix(kFold_evals[["ubcf-p"]])
CFM_ubcf_j = getConfusionMatrix(kFold_evals[["ubcf-j"]])
CFM_ibcf_c = getConfusionMatrix(kFold_evals[["ibcf-c"]])
CFM_ibcf_p = getConfusionMatrix(kFold_evals[["ibcf-p"]])
CFM_ibcf_j = getConfusionMatrix(kFold_evals[["ibcf-j"]])
# CFM_ubcf_c =getConfusionMatrix(kFold_evals[["ubcf-c-centered"]])
# CFM_ubcf_p =getConfusionMatrix(kFold_evals[["ubcf-p-centered"]])
# CFM_ubcf_j =getConfusionMatrix(kFold_evals[["ubcf-j-centered"]])
# CFM_ibcf_c =getConfusionMatrix(kFold_evals[["ibcf-c-centered"]])
# CFM_ibcf_p =getConfusionMatrix(kFold_evals[["ibcf-p-centered"]])
# CFM_ibcf_j =getConfusionMatrix(kFold_evals[["ibcf-j-centered"]])
CFM_svd =getConfusionMatrix(kFold_evals[["svd"]])
#CFM_pca =getConfusionMatrix(kFold_evals[["pca"]])

#Get average value of k-fold
avg(kFold_evals[["random"]])
avg(kFold_evals[["popular"]])
avg(kFold_evals[["ubcf-c"]])
avg(kFold_evals[["ubcf-p"]])
avg(kFold_evals[["ubcf-j"]])
avg(kFold_evals[["ibcf-c"]])
avg(kFold_evals[["ibcf-p"]])
avg(kFold_evals[["ibcf-j"]])
avg(kFold_evals[["svd"]])

#x=random_avg[,"precision"]
#y=popular_avg[,"precision"]
#wilcox.test(y,x,alternative = "greater")

# convert matrix to table
# popular_result_table = as.data.frame(matrix(popular_result[[10]],nrow=6, ncol=8, dimnames=list(c("1","3","5","10","15","20"),c("TP","FP","FN","TN","precision","recall","TPR","FPR"))))


# visualization
# ROC Curve
pdf("Desktop/Recommender/pic/roc_min=10.pdf")
plot(kFold_evals, annotate=1:length(algorithms), legend="topleft")
dev.off()
# can't draw, since each CFM contains NaN. After adjusting minUserReviews and goodRating we get the diagram.
# plot prec/recall chart
pdf("Desktop/Recommender/pic/pr_min=10.pdf")
plot(kFold_evals, "prec/rec", annotate=1:length(algorithms), legend="topleft")
dev.off()
# TODO: Evaluate Predictions (RMSE)
# TODO: Evaluate Binarized data
# TODO: Build a script that can make a prediction (top 5) and retrieve the restaurant names for a user

# MAE
random_avg = avg(kFold_evals[["random"]])
random_mae= (random_avg[,"FP"]+ random_avg[,"FN"])/(random_avg[,"FP"]+random_avg[,"FN"]+random_avg[,"TN"]+1-random_avg[,"TP"])

# --- misc functions ---
# simple search for value
# yelp_rating_unique[yelp_rating_unique$user_id == 'ZzMIcp1Ev47C3owOnjwQ9Q',]