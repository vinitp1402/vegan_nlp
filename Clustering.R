read.csv("all_tweets_3.csv") -> tw
read.csv("user_ds_cleaned.v.3.0.csv") -> usr_tw

library(sqldf)

merge(tw, usr_tw) -> merged_df

merged_df[,c(5:7,9:12)] -> cls_df

scale(cls_df) -> scaled_cls_df

dim(cls_df)

dim(scaled_cls_df)

dist(scaled_cls_df, method = "euclidean")

wss <- (nrow(scaled_cls_df)-1)*sum(apply(scaled_cls_df,2,var))

fit <- kmeans(na.omit(scaled_cls_df), 5)
aggregate(scaled_cls_df,by=list(fit$cluster),FUN=mean)
