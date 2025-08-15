# compute word feature correlation and mutual information with hits
#
# MPMI computes mutual information
library("mpmi");
#
# load word data
df=read.csv("allscores_word.csv");
print(summary(df))
#
# average over repeats of word in scene
dm=aggregate(cbind(HIT,LOGTRIPROB,PWPOS,NWORD)~SCENEWORD,data=df,FUN=mean);
print(summary(dm))
# average over word
dw=aggregate(cbind(HIT,LOGWFREQ,PCOUNT,SCOUNT,NCOUNT)~WORD,data=df,FUN=mean);
print(summary(dw))

feats=c("STOI","RMSE","CORR")

for (f in feats) {
	cat(sprintf("=================================== %s\n",f));
	flush.console();
	subset=seq(1,nrow(df),10);
	corr=cor(df$HIT[subset],df[subset,f])
	mi=cmi.pw(df$HIT[subset],df[subset,f]);
	cat(sprintf("%s: r=%.3f bcmi=%.3f\n",f,corr,mi$bcmi));
}

feats=c("LOGTRIPROB","PWPOS","NWORD")

for (f in feats) {
	cat(sprintf("=================================== %s\n",f));
	flush.console();
	corr=cor(dm$HIT,dm[,f])
	mi=cmi.pw(dm$HIT,dm[,f]);
	cat(sprintf("%s: r=%.3f bcmi=%.3f\n",f,corr,mi$bcmi));
}

feats=c("LOGWFREQ","PCOUNT","SCOUNT","NCOUNT")

for (f in feats) {
	cat(sprintf("=================================== %s\n",f));
	flush.console();
	corr=cor(dw$HIT,dw[,f])
	mi=cmi.pw(dw$HIT,dw[,f]);
	cat(sprintf("%s: r=%.3f bcmi=%.3f\n",f,corr,mi$bcmi));
}
