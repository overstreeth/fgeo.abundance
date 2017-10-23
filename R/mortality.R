# For documentation see demography.R

#' @rdname demography
#' @export
mortality <- function(census1,
                           census2,
                           alivecode = c("A", "AB", "AS"),
                           split1 = NULL,
                           split2 = NULL) {
  check_crucial_names(census1, nms = c("dbh", "pom", "status", "date"))
  check_if_all_dbh_is_na(census1, census2, split = split1)
  check_if_all_dbh_is_na(census1, census2, split = split2)

 if(is.null(split1)) split1=rep("all",dim(census1)[1])
 if(is.null(split2)) split2=rep("all",dim(census2)[1])

 inc=!is.na(census1$status) & !is.na(census2$status) & census1$status!="M" & census2$status!="M"
 census1=census1[inc,]
 census2=census2[inc,]
 split1=split1[inc]
 split2=split2[inc]

 time=(census2$date-census1$date)/365.25

 alive1=alive2=rep(FALSE,dim(census1)[1])
 alive1[census1$status=="A"]=TRUE
 for(i in 1:length(alivecode)) alive2[census2$status==alivecode[i]]=TRUE

 class1=sort(unique(split1))
 class2=sort(unique(split2))
 splitN=list(split1[alive1],split2[alive1])
 splitS=list(split1[alive1&alive2],split2[alive1&alive2])

 N=tapply(census1$dbh[alive1],splitN,length)
 S=tapply(census1$dbh[alive1&alive2],splitS,length)
 meantime=tapply(time[alive1],splitN,mean,na.rm=T)
 meandbh=tapply(census1$dbh[alive1],splitN,mean,na.rm=T)
 startdate=tapply(census1$date[alive1],splitN,mean,na.rm=T)
 enddate=tapply(census2$date[alive1],splitN,mean,na.rm=T)

 N=fill.dimension(N,class1,class2)
 S=fill.dimension(S,class1,class2)
 meantime=fill.dimension(meantime,class1,class2,fill=NA)
 meandbh=fill.dimension(meandbh,class1,class2,fill=NA)
 startdate=fill.dimension(startdate,class1,class2,fill=NA)
 enddate=fill.dimension(enddate,class1,class2,fill=NA)

 if(sum(N)==0)
   return(list(N=rep(NA,length(class1)),D=rep(NA,length(class1)),
               rate=rep(NA,length(class1)),
               lower=rep(NA,length(class1)),upper=rep(NA,length(class1)),
               time=rep(NA,length(class1)),dbhmean=rep(NA,length(class1)),
               date1=rep(NA,length(class1)),date2=rep(NA,length(class1))
              )
         )

 m=mortality.calculation(N=as.matrix(N),S=as.matrix(S),meantime=as.matrix(meantime))

 # ord=order(drp(meandbh))
 result=list(N=drp(m$N),D=drp(m$D),rate=drp(m$rate),lower=drp(m$lowerCI),upper=drp(m$upperCI),time=drp(m$time),
             date1=drp(startdate),date2=drp(enddate),dbhmean=drp(meandbh))

 return(result)
}

#' @rdname demography
#' @export

mortality_df <- function(census1, 
                         census2, 
                         alivecode = c("A", "AB", "AS"),
                         split1 = NULL) {
  result <- mortality(census1 = census1, census2 = census2, 
    alivecode = alivecode, split1 = split1)
  demography_df(result)
}



# Internal dependencies of recruitment ------------------------------------

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
mortality.calculation=function(N,S,meantime)
{
 lower.ci=find.climits(N,(N-S),kind="lower")
 upper.ci=find.climits(N,(N-S),kind="upper")

 mort.rate=(log(N)-log(S))/meantime
 upper.rate=(log(N)-log(N-upper.ci))/meantime
 lower.rate=(log(N)-log(N-lower.ci))/meantime

 mort.rate[S==0]=upper.rate[S==0]=Inf
 upper.rate[upper.ci==N]=Inf
 lower.rate[lower.ci==N]=0
 mort.rate[N==0]=lower.rate[N==0]=upper.rate[N==0]=NA

 if(is.null(dim(N)))
   return(data.frame(N=N,S=S,D=N-S,rate=mort.rate,lowerCI=lower.rate,upperCI=upper.rate,
                     time=meantime))
 else
   return(list(N=N,S=S,D=N-S,rate=mort.rate,lowerCI=lower.rate,upperCI=upper.rate,
               time=meantime))
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
calcMortIndivTable=function(mtable,by='species')
{
 if(by=='species') splitby=list(mtable$sp,mtable$census)
 else splitby=mtable$census

 S=tapply(mtable$fate,splitby,sum)
 S[is.na(S)]=0
 N=tapply(mtable$fate,splitby,length)
 N[is.na(N)]=0
 time=tapply(mtable$interval,splitby,mean,na.rm=TRUE)

 mortality=mortality.calculation(N,S,time)$rate
 return(mortality)
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
mortality.eachspp=function(census1,census2,classbreak=c(10,100,300),alivecode=c("A","AB","AS"))
{
 allbreak=c(classbreak,10000)
 dbhclass=as.numeric(as.character(cut(census1$dbh,breaks=allbreak,right=F,labels=classbreak)))

 sp=census1$sp
 result=mortality(census1,census2,alivecode=alivecode,split1=sp,split2=dbhclass)
 return(result)
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
mortality.dbh=function(census1,census2,classbreak=c(10,100,300),alivecode=c("A","AB","AS"))
{
 allbreak=c(classbreak,10000)
 dbhclass=as.numeric(as.character(cut(census1$dbh,breaks=allbreak,right=F,labels=classbreak)))

 result=mortality(census1,census2,alivecode=alivecode,split2=dbhclass)
 ord=order(result$dbhmean)
 for(i in 1:length(result)) result[[i]]=result[[i]][ord]

 return(result)
}
