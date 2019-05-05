#' initinherit
#'
#' a generic function to initialize object
#' @param Object an object
#' @param ... the object or parameters from which inheritance is transmitted
#' @return the update object
#'
#' @export
initinherit<-function(Object,...){
  #Object <- callNextMethod()
  dots<-list(...)
  if(length(dots)>0){
  	testusedots<-lapply(dots,function(a){a<-FALSE})
	#class inheritance in value
	for(namedots in names(dots)){
		#class to class
		#print(namedots)
		if(inherits(Object,namedots)){
			#print("class")
 			testusedots[[namedots]]<-TRUE
  			slotobj<-methods::slotNames(Object)
			slotdots<-methods::slotNames(dots[[namedots]])
			for(idslot in slotdots){
				#print(idslot)
				methods::slot(Object,idslot)<-methods::slot(dots[[namedots]],idslot)
			}
		}
		#slot to slot
		if(any(slotNames(Object)%in%namedots)){
			#print("slot")
 			testusedots[[namedots]]<-TRUE
			methods::slot(Object,namedots)<-dots[[namedots]]
		}
	}
	wrongdots<-unlist(testusedots)
	wrongdots<-names(wrongdots)[!wrongdots]
#print(wrongdots)
	#print(unlist(testusedots))
	#if(any(unlist(testusedots))){
	if(length(wrongdots)>0){
		wrongdots<-unlist(testusedots)
		wrongdots<-names(wrongdots)[!wrongdots]
		warning(paste0("parameters ",paste0(wrongdots,collapse=",")," unknown not used"))
	}
  }
  return(Object)
}


#define the methods
setMethod("plot","Time",
	  function(x,...){
		  #nomslot<-methods::slotNames(object)
		  plot(x=x@TimeDate,y=rep(0,length(x@TimeDate)),xlab="TimeDate",ylab="",...)#,y=x@TimeType)
	  }
	  )
#setMethod("show","Time",
#	  function(object){
#		  cat(paste0(length(object)," time objects\n"))
#		  cat(paste0(object@TimeDate," ",object@TimeType,"\n"))
#	  }
#	  )
if(F){
	library("CLEFRDB")
	haultime<-c("2011-03-27 01:30:03",
		                "2011-03-27 12:00:00",
				            "2011-03-15 12:00:00",
				            "2011-02-14 00:00:00")
	haultime<-as.POSIXct(strptime(haultime,
				                                 "%Y-%m-%d %H:%M:%S"))
	haultime<-new("Time",TimeType=c("date","day","month","quarter"),
		          TimeDate=haultime)
	print(haultime)
setClass(Class="Landings",
	 slots=c("w"="numeric"),
	 contains=c("Time"),
	 prototype=prototype(Landings=numeric(),
			     Time=new("Time"))
	 )

new("Landings")
new("Landings",w=rnorm(4),TimeDate=haultime@TimeDate,TimeType=haultime@TimeType)

setMethod("initialize","Landings",function(.Object){.Object<-initinherit(.Object);return(.Object)})
new("Landings")
new("Landings",w=rnorm(4),Time=haultime)

initinherit(new("Time"),Time=haultime,pipo=10)
aa<-initinherit(new("Landings"),Landings=c(10,20,50,60),Time=haultime)
new("Landings",Landings=rnorm(4))#,Time=haultime)
new("Trip",Time=haultime)


}
