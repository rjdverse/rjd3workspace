#' Deprecated functions
#'
#'
#' @param jmp,idx,jws,name Parameters.
#' @name deprecated-rjdemetra3
#' @export
.jmp_sa_count<-function(jmp){
  .Deprecated(".jsap_sa_count")
  .jsap_sa_count(jmp)
}


#' @name deprecated-rjdemetra3
#' @export
.jmp_name<-function(jmp){
  .Deprecated(".jsap_name")
  .jsap_name(jmp)
}

#' @name deprecated-rjdemetra3
#' @export
.jmp_sa<-function(jmp, idx){
  .Deprecated(".jsap_sa")
  .jsap_sa(jmp, idx)
  }
#' @name deprecated-rjdemetra3
#' @export
.jmp_sa_name <- function(jmp) {
  .Deprecated(".jsap_sa_name")
  .jsap_sa_name(jmp)
}

#' @name deprecated-rjdemetra3
#' @export
.jmp_load<-function(jmp){
  .Deprecated("read_sap")
  read_sap(jmp)
}
#' @name deprecated-rjdemetra3
#' @export
.jws_multiprocessing<-function(jws, idx){
  .Deprecated(".jws_sap")
  .jws_sap(jws, idx)
}
#' @name deprecated-rjdemetra3
#' @export
.jws_multiprocessing_new<-function(jws, name){
  .Deprecated(".jws_sap_new")
  .jws_sap_new(jws, name)
}
#' @name deprecated-rjdemetra3
#' @export
.jws_multiprocessing_count<-function(jws){
  .Deprecated(".jws_sap_count")
  .jws_sap_count(jws)
}
