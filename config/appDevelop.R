library(dplyr)
library(pROC)
library(caret)

app = function(model, algorithm, valDT) {

# valDT=read.table("/media/thong/sda/RCID/CoHoai/Tuberculosis/ShinyR/model/example.csv", header=T, sep=",")
rownames(valDT)=valDT$Sample
dt=valDT[,grep("cg", names(valDT))]

CpG=read.table(paste0("model/", model, ".txt"), header=T, sep="\t")$CpGs

REQ=data.frame(Required=CpG)
UI=data.frame(RequiredInput=names(dt), UserInput=names(dt))

check=merge(REQ, UI, by.x="Required", by.y="RequiredInput", all.x=T)
n=sum(is.na(check$UserInput))

if (n>0) {
check[is.na(check$UserInput), "UserInput"] = "Missing value in this CpG, check your file"
print(check)
} else if (n==0) {

select=paste0(model, "_", algorithm)

rf.m1=readRDS(paste0("model/", select, ".rds"))

pre.m1s <- predict(rf.m1, dt, type="prob")

pre.m1s$predict <- factor(ifelse(pre.m1s$HC >= .5, "Normal", "TB"))

outDF=cbind(pre.m1s, dt)
outDF$Sample=rownames(outDF)

outDF=outDF[,c("Sample", "predict", CpG)]
rownames(outDF)=NULL
print(outDF)

}

}
