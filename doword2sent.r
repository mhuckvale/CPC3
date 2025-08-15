# convert word scores back to sentence scores and build intelligibility model
#
# convert prob to logits
prob2logit <- function(prob){
	prob=ifelse(prob <= 0.05,0.05,prob);
	prob=ifelse(prob >= 0.95,0.95,prob);
    return(log(prob/(1-prob)));
}
#
# convert logits to scores
logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
}
#
# cross-validated scores from random forest classifier on training data
df=read.csv("trainforestpred_word3.csv",stringsAsFactors=F)
#
for (i in 1:nrow(df)) {
	df$SIGNAL[i] = substr(df$SIGNAL[i],1,22)
}
df$SIGNAL=factor(df$SIGNAL)
df$LOGPREDICT=prob2logit(df$PREDICT)
#
df=aggregate(cbind(PREDICT,LOGPREDICT)~SIGNAL,data=df,FUN=mean)
#
# read metadata scores
scores=read.csv("allscores.csv");
df=merge(scores,df,by="SIGNAL");
df$PCORRECT=df$CORRECT/100;
print(summary(df))
#
# cross-validated logistic regression
runregression=function(df,formula) {
	df$GROUP=1+(seq(1,nrow(df)) %% 5);
	df$PREDICT=NA
	for (group in 1:5) {
		trainidx=which(df$GROUP!=group);
		testidx=which(df$GROUP==group);
		
		m=glm(as.formula(formula),data=df[trainidx,],family=quasibinomial(link='logit'));
		
		df$PREDICT[testidx]=logit2prob(predict(m,df[testidx,]));
	}
	return(df$PREDICT)
}
#
# plot graphs
par(mfrow=c(1,2),cex=1);

df$PREDSCORE=runregression(df,"PCORRECT~LOGPREDICT")*100;

corr=cor(df$PREDSCORE,df$CORRECT)
rmse=sqrt(mean((df$PREDSCORE-df$CORRECT)^2))
plot(df$PREDSCORE~df$CORRECT,main=sprintf("~PREDICT r=%.3f rmse=%.3f",corr,rmse),pch=19,col=rgb(0,0,1,0.1),xlab="%Correct",ylab="Predicted %Correct")
grid();

df$PREDSCORE=runregression(df,"PCORRECT~0+LOGPREDICT+SEVERITY")*100;

corr=cor(df$PREDSCORE,df$CORRECT)
rmse=sqrt(mean((df$PREDSCORE-df$CORRECT)^2))
plot(df$PREDSCORE~df$CORRECT,main=sprintf("~PREDICT+SEVERITY r=%.3f rmse=%.3f",corr,rmse),pch=19,col=rgb(0,0,1,0.1),xlab="%Correct",ylab="Predicted %Correct")
grid();

par(mfrow=c(1,1))
