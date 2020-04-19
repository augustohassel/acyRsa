#' @export token
#' @export base_url
#' @export token<-
#' @export base_url<-
NULL

# Connection Class
setClass("acyRsaConnection",
         slots = c(
           token = "character",
           base_url = "character",
           valid_until = "character"
         ),
         prototype = list(
           token = NA_character_,
           base_url = NA_character_,
           valid_until = as.character(Sys.Date())
         )
)

setValidity("acyRsaConnection", function(object){
  if (length(object@token) != 1 || length(object@base_url) != 1) {
    "@token and @base_url must have lenght 1"
  } else if (object@token == "" || object@base_url == "") {
    "@token and @base_url can not be empty"
  } else {
    TRUE
  }
})

setGeneric("token", function(x) standardGeneric("token"))
setMethod("token", "acyRsaConnection", function(x) x@token)

setGeneric("token<-", function(x, value) standardGeneric("token<-"))
setMethod("token<-", "acyRsaConnection", function(x, value) {
  x@token <- value
  validObject(x)
  x
})

setGeneric("base_url", function(x) standardGeneric("base_url"))
setMethod("base_url", "acyRsaConnection", function(x) x@base_url)

setGeneric("base_url<-", function(x, value) standardGeneric("base_url<-"))
setMethod("base_url<-", "acyRsaConnection", function(x, value) {
  x@base_url <- value
  validObject(x)
  x
})
