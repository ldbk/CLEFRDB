#' Validity space method
#'
#' @param object a space object
#' 
validSpace<-function(object){
	#SpaceType<-NULL
	#utils::data(SpaceType,package="fishpi2qc")
	#print(SpaceType)
		check<-TRUE
		if(length(object@SpacePlace) == length(object@SpaceType)){
			check<-TRUE&check
		}else{
			print(paste0("length(SpacePlace)!=length(SpaceType)"))
			check<-FALSE&check
		}
		if(all(object@SpacePlace%in%defspace$id)){
			check<-TRUE&check
		}else{
			id<-which(!object@SpacePlace%in%defspace$id)
			print(paste0("wrong Space SpacePlace at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
		if(all(object@SpaceType%in%Spacetype)){
			check<-TRUE&check
		}else{
			id<-which(!object@SpaceType%in%Spacetype)
			print(paste0("wrong Space SpaceType at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
		return(check)

}
#' Class Space 
#'
#' @slot SpacePlace 
#' @slot SpaceType 
#'
setClass(Class="Space",
	slots=c(SpaceType="character",SpacePlace="character"),
	prototype=prototype(SpacePlace=character(),SpaceType=character()),
	validity=validSpace
	)

if(F){
library(CLEFRDB)
new("Space")
new("Space",SpacePlace="27.7.h",SpaceType="ICESdiv")
new("Space",SpacePlace=c("27.7.h","GSA07","FRRTB"),SpaceType=c("ICESdiv","GSA","harbour"))
new("Space",SpacePlace=c("27.7.h","GSA07"),SpaceType=c("ICESdiv","GSA"))
new("Space",SpacePlace=c("27.7.h","GSA078"),SpaceType=c("ICESdiv","GSA"))
new("Space",SpacePlace=c("27.7.h","GSA07","FRRTB","DEBRB"),SpaceType=c("ICESdiv","GSA","harbour"))
}
