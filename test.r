

help("openxlsx")
install.packages("mgcv")
install.packages("lmerTest")
resultdata <- read.xlsx("多项式混合效应模型结果.xlsx")
origindata <- read.xlsx("long.xlsx")
resultdata <- as_tibble(resultdata)
origindata <- as_tibble(origindata)
resultdata$b0
resultdata[["b0"]]
resultdata[2]
a <- c(1,2,3,4)
length(a)
str(origindata)
origindata <- read_xlsx("long.xlsx")
while(TRUE){x=rnorm(1) 
print(x) 
if(x>1) break}
help(write)
repeat{
  x=rnorm(1) 
  print(x)  
  if(x>1) break}
origindata["fctag117"] <- factor(origindata["ag117"])
origindata["fctag117"] <- as.factor(origindata["ag117"])
tapply(origindata["wei"],origindata["ag117"],mean)
sapply(origindata,mean)
sapply(origindata,mean,simplify=FALSE)
lapply(origindata,mean)
map(origindata,mean)
map(origindata,~mean(.x))
df <- tibble(x=c(1,3,5),y=c(5,10,2),z=c(1,2,3))
df <- tibble(n=c(1,3,5),mean=c(5,10,2),sd=c(1,2,3))
pmap(df,rnorm)
pmap(df,~rnorm(n=1,mean=0,sd=1))
pmap(df,~rnorm(..1,..2,..3))
pmap(df,~ rnorm(...))
pmap(df,mean)
pmap(df,~mean(c(..1,..2)))
pmap(df,~mean(c(...)))
help(pmap)
help(group_by)
df = tibble(
  x = 1:6, y = c("A","A","B","B","C","C"),
  z = c(2.13,3.65,1.88,2.30,6.55,4.21))
df
df %>% pivot_wider()
help(group_by)
help(mutate)
help(summarise)
help(slide_db1)
help(slider)
install
help(tidy)
help(broom)
help(lme4)
help(lmer)
help(augment)
origindata=origindata %>% mutate(pc=as.factor(pc))
str(origindata)
model5 <- lmer(bmi~monthage+(1+monthage|childid),data=origindata)#随机斜率和截距
summary(model5)
ranef(model5)
model6 <- gamm(formula(bmi~s(monthage,bs='cr')),
               random=list(childid=~1),data=origindata,
               family=gaussian)
pred<-predict.gam(model6$gam,type="terms",na.action = na.pass)
help(predict.gam)
mfit <- pred$fit[,'s(monthage)']
pred$fit
origindata6 <- cbind(origindata,mfit)
plot(mfit~monthage,ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),col=col[1],type="l", lty=1, lwd=2, ylab="", xlab="")