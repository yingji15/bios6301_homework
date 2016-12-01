set.seed(111)
n<-1e3
x<-runif(n,max=n)
match<-sample(n)
head(match)

'%d%'<-function(i,j) abs(i-j)
1 %d% 10
10 %d% 1

getMatches1<-function(x,match){
        n<-length(x)
        dist<-outer(x,x,'%d%')
        matches<-data.frame(row1=numeric(n),row2=numeric(n),distance=numeric(n))
        for (i in seq.int(n)){
                matches[i,]<-c(i,match[i],dist[i,match[i]])
        }
        matches
}
m<-getMatches1(x,match)
head(m)
system.time(getMatches1(x,match))
install.packages('microbenchmark')
library(microbenchmark)

microbenchmark(getMatches1(x,match)) #neval: number of evaluations, summarize time

set.seed(111)
n<-1e4
x<-runif(n,max=n)
match<-sample(n)
Rprof()
n<-getMatches1(x,match)
Rprof(NULL)
summaryRprof() #by.total outer and data frame assignment do almost all

getMatches2<-function(x,match){
        n<-length(x)
        
        dist<-lapply(x,"%d%",x)
        matches<-data.frame(row1=seq.int(n),row2=match,distance=numeric(n))
        for (i in seq.int(n)){
                matches[i,'distance']<-dist[[i]][match[i]]
        }
        matches
}

getMatches3<-function(x,match){
        n<-length(x)
        dist<-outer(x,x,'%d%')
        i<-seq.int(n)
        data.frame(row1=i,row2=match,distance=dist[cbind(i,match)])
}

getMatches4<-function(x,match){
        n<-length(x)
        dist<-lapply(x,'%d%',x)
        zm<-mapply('[',dist,match) #[  extract value use index
        i<-seq.int(n)
        data.frame(row1=i,row2=match,distance=zm)
}
seq(5)[4]
'['(seq(5),4)
 
gc() #garbage collection
set.seed(111)
n<-1e3
x<-runif(n,max=n)
microbenchmark(
        getMatches1(x,match),
        getMatches2(x,match),
        getMatches3(x,match),
        getMatches4(x,match)
)

set.seed(111)
n<-1e4
x<-runif(n,max=n)
match<-sample(n)
Rprof()
n<-getMatches4(x,match)
Rprof(NULL)
summaryRprof()
