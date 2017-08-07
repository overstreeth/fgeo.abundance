#' Takes row and column numbers and identifies the quadrate number (index).
#'
#' @description
#' Takes row and column numbers and identifies the quadrate number (index). The 
#' row and column numbers are based on a `gridsize` that by default divides the 
#' plot into 20 by 20 m squares. The g`ridsize` can be defined by the user so 
#' other quadrate sizes can be used.
#' 
#' @inheritParams findborderquads
#' @template gridsize_side
#' @param rowno Row number.
#' @param colno Column number.
#'
'rowcol.to.index'

rowcol.to.index=function(rowno,colno,gridsize=20,plotdim=c(1000,500))
{
 badrc=(rowno<=0 | colno<=0 | rowno>plotdim[2]/gridsize | colno>plotdim[1]/gridsize)

 rowno=rowno-1
 colno=colno-1
 maxrow=floor(plotdim[2]/gridsize)
 index=colno*maxrow+rowno+1
 if(length(badrc[badrc>0])) index[badrc]=NA
 return(index)
}



#' Assign any location(s) a single index identifying the quadrat.
#'
#' @description
#'
#' Assign any location(s) a single index identifying the quadrat. The index runs
#' from 1 to the number of quadrats.
#'
#' @inheritParams gxgy.to.hectindex
#' @inheritParams findborderquads
#'
'gxgy.to.index'

gxgy.to.index=function(gx,gy,gridsize=20,plotdim=c(1000,500))
{
 badgxgy=(gx<0 | gy<0 | gx>=plotdim[1] | gy>=plotdim[2] | is.na(gx) | is.na(gy))

 colno=1+floor(gx/gridsize)
 rowno=1+floor(gy/gridsize)
 if(length(badgxgy[badgxgy>0])) colno[badgxgy]=rowno[badgxgy]=NA

 return(rowcol.to.index(rowno,colno,gridsize,plotdim))
}



#' Abundance, basal area, or agb of every species by quadrat.
#'
#' @description
#' Finds abundance, basal area, or agb of every species per square quadrat of
#' any size; plotdim is the x dimension then y dimension of the plot and must be
#' set correctly; gridsize is the quadrat dimension. The plot is divided into a
#' checkerboard of non-overlapping, space-filling squares.
#'
#' @details
#' If the plot dimensions is not an exact multiple of the quadrat size, then a 
#' strip at the upper edge of the plot (north and east if plot is on cardinal 
#' directions) is omitted. For example, if `gridsize = 40` and `plotdim = 500`,
#' then there are an extra 20 meters at the upper boundary omitted from the 
#' calculations.
#'
#' The array of abundances per quadrat is useful for similarity, counting
#' species and stems per quadrat, etc.
#' 
#' @inheritParams abundance
#' @inheritParams findborderquads
#' 
#' @seealso [abundance()]
#' 
#' @return See [abundance()].
#'
#' @examples
#' \dontrun{
#' Nperquad = abundanceperquad(
#'   bci::bci12full6,
#'   plotdim = c(1000, 500),
#'   gridsize = 100,
#'   type = 'abund'
#' )
#' colSums(Nperquad$abund)
#' apply(Nperquad$abund, 2, countspp)
#' plot(colSums(Nperquad$abund), apply / (Nperquad$abund, 2, countspp))
#' }
#'
'abundanceperquad'

abundanceperquad=function(censdata,mindbh=10,plotdim=c(1000,500),gridsize=100,type='abund',dbhunit='mm')
{
 sp=censdata$sp

 quadno=gxgy.to.index(censdata$gx,censdata$gy,gridsize=gridsize,plotdim=plotdim)
 result=abundance(censdata,type=type,mindbh=mindbh,dbhunit=dbhunit,split1=sp,split2=quadno)

 allspp=unique(censdata$sp)
 maxquad=floor(plotdim[1]/gridsize)*floor(plotdim[2]/gridsize)
 allquad=1:maxquad

 if(dim(result[[type]])[1]<length(allspp) | dim(result[[type]])[2]<length(allquad))
     result[[type]]=fill.dimension(result[[type]],class1=allspp,class2=allquad,fill=0)
  
 return(result)
}



#' Abundance or basal area dividing data with 1 or 2 categorical variables
#' 
#' @description
#' Calculates total abundance or basal area, dividing data with 1 or 2 
#' categorical variables.
#' 
#' @details
#' The categorical variables must be submitted as vectors whose length matches
#' exactly as the number of rows in the plot data submitted (so one per tree or
#' stem). The first vector should be the one with the most categories (for
#' instances, `split1 = species`, `split2 = dbhcategory`).
#' 
#' For basal area, pass a stem table to `censdata`; For abundance, use either
#' the stem or full table to count stems or trees, respectively.
#' 
#' @template mindbh
#' @template censdata
#' @template type
#' @template alivecode
#' @template dbhunit
#' @param split1 a vector of categories, one per individual
#' @param split2 another vector of categories, one per individual
#'
#' @return a named list of two elements, either `abund` or `ba`, and meandate 
#'   (mean measurement date for all individuals in each category). Each element 
#'   of the list is an array of one or two dimensions, depending on how many 
#'   split variables were submitted: each of the dimensions of the array handles
#'   one of the categorical variables.
#'
#' @examples
#' \dontrun{
#'
#' CTFSplot('bci',5:6,'full')
#'
#' CTFSplot('bci',5:6,'stem')
#' total=abundance(bci::bci12full5,mindbh=10)
#' total$abund
#' total$meandate
#' totalstem=abundance(bci::bci12stem5,mindbh=10)
#'
#' BAperSpecies=abundance(bci::bci12stem5,type='ba',mindbh=10,split1=bci::bci12stem5$sp)
#' head(BAperSpecies$ba)
#' head(BAperSpecies$meandate)}
#'
#'
'abundance'

abundance <- function(censdata,type='abund',alivecode=c("A"),mindbh=NULL,dbhunit='mm',split1=NULL,split2=NULL)
{
 if(is.null(split1)) split1=rep("all",dim(censdata)[1])
 if(is.null(split2)) split2=rep("all",dim(censdata)[1])

 if(!is.null(mindbh)) inc=censdata$dbh>=mindbh
 else inc=rep(TRUE,length(censdata$dbh))
 
 alive=rep(FALSE,length(censdata$dbh))
 for(i in 1:length(alivecode)) alive[censdata$status==alivecode[i]]=TRUE

 class1=sort(unique(split1))
 class2=sort(unique(split2))
 groupvar=list(split1[alive&inc],split2[alive&inc])
 
 if(type=='abund') abund=tapply(censdata$dbh[alive&inc],groupvar,length)
 else if(type=='ba') abund=tapply(censdata$dbh[alive&inc],groupvar,basum,mindbh=mindbh,dbhunit=dbhunit)
 else if(type=='agb') abund=tapply(censdata$agb[alive&inc],groupvar,sum,na.rm=TRUE)
 # browser()
 
 meandate=tapply(censdata$date[alive&inc],groupvar,mean,na.rm=TRUE)
 
 abund=fill.dimension(abund,class1,class2)
 meandate=fill.dimension(meandate,class1,class2,fill=NA)

 result=list(abund=abund,meandate=meandate)
 if(type=='ba') names(result)[1]='ba'
 else if(type=='agb') names(result)[1]='agb'
 
 return(result)
}



#' This function fills out an array of 2 dimensions, adding zeroes (or...
#'
#' @description
#'
#' This function fills out an array of 2 dimensions, adding zeroes (or other values) for extra columns
#' and rows as named in class1 and class2. If a column (or row) is
#' missing, it will be filled with the value given by fill. It is useful for results of table
#' or tapply when some elements had no records. 
#'
#'
'fill.dimension'

fill.dimension=function(dataarray,class1,class2,fill=0)
{
 result=data.frame(matrix(fill,nrow=length(class1),ncol=length(class2)))
 rownames(result)=class1
 colnames(result)=class2

 result[rownames(dataarray),colnames(dataarray)]=dataarray
 result[is.na(result)]=fill
 
 return(result)
}



#' Returns the basal area summed over all submitted dbhs.
#'
#' @description
#' Returns the basal area summed over all submitted dbhs. NAs can be included,
#' as sum will be completed with `na.rm = TRUE`.
#'
#' @template dbh
#' @template mindbh
#' @template dbhunit
#'
'basum'

basum=function(dbh,mindbh=10,dbhunit='mm')
{
 if(!is.null(mindbh)) dbh=dbh[dbh>=mindbh]

 if(length(dbh)==0) return(0)
 return(sum(ba(dbh,dbhunit=dbhunit),na.rm=TRUE))
}



#' Basal area of trees.
#'
#' @description 
#' Calculates the individual basal areas (in square meters) for all submitted
#' dbhs. The dbh units must be submitted, either cm'or 'millimeters'.
#' @inheritParams abundance
#' @template dbh
#' @return A vector of basal area values of same length as the submitted vector 
#'   of dbhs.
'ba'

ba=function(dbh,dbhunit='mm') 
 {
  if(dbhunit=='mm') return(pi*(dbh/2000)^2)
  if(dbhunit=='cm') return(pi*(dbh/200)^2)
 }
