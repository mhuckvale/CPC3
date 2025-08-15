# compute word feature correlation and mutual information with hits
#
# MPMI computes mutual information
library("mpmi");
#
# load word data
df=read.csv("allscores_word.csv");
print(summary(df))
#

dw=aggregate(cbind(HIT,LOGTRIPROB,LOGWFREQ,PWPOS,PCOUNT,SCOUNT,NCOUNT,NWORD)~WORD,data=df,FUN=mean);
print(summary(dw))

feats=c("STOI","RMSE","CORR")

for (f in feats) {
	cat(sprintf("\n=================================== %s\n",f));
	flush.console();
	subset=seq(1,nrow(df),5)			# too many rows otherwise
	corr=cor(df$HIT[subset],df[subset,f])
	mi=cmi.pw(df$HIT[subset],df[subset,f]);
	cat(sprintf("%s: r=%.3f bcmi=%.3f\n",f,corr,mi$bcmi));
}

feats=c("LOGTRIPROB","LOGWFREQ","PWPOS","PCOUNT","SCOUNT","NCOUNT","NWORD")

for (f in feats) {
	cat(sprintf("\n=================================== %s\n",f));
	flush.console();
	corr=cor(dw$HIT,dw[,f])
	mi=cmi.pw(dw$HIT,dw[,f]);
	cat(sprintf("%s: r=%.3f bcmi=%.3f\n",f,corr,mi$bcmi));
}
