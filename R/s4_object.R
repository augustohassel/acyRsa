#' @export token
#' @export base_url
#' @export valid_until
#' @export agent
#' @export user_name
NULL

#' @title acyRsaConnection-method
#' @name acyRsaConnection-method
#' @rdname acyRsaConnection-method
#' @param x S4 Class. acyRsaConnection object
#' @param object S4 Class. acyRsaConnection object
NULL

# class definition -----

#' @title Connection Class: acyRsaConnection
#'
#' @description Creates a acyRsa connection object
#'
#' @slot token character. Obtained from login method
#' @slot base_url character. Connected environment
#' @slot valid_until character. Log-in date. Valid for a day.
#' @slot agent character. User Agent to pass to the API. Format: 'acyRsa-<environment>-user_name'
#' @slot user_name character. User Name.
setClass("acyRsaConnection",
         slots = c(
           token = "character",
           base_url = "character",
           valid_until = "character",
           agent = "character",
           user_name = "character"
         ),
         prototype = list(
           token = NA_character_,
           base_url = NA_character_,
           valid_until = NA_character_,
           agent = NA_character_,
           user_name = NA_character_
         )
)

# class validation ----

setValidity("acyRsaConnection", function(object){
  if (length(object@token) != 1 || length(object@base_url) != 1 || length(object@agent) != 1 || length(object@user_name) != 1) {
    "@token, @base_url, @agent and @user_name must have lenght 1"
  } else if (object@token == "" || object@base_url == "" || object@agent == "" || object@user_name == "") {
    "@token, @base_url, @agent and @user_name can not be empty"
  } else if (!grepl(pattern = "^(http|https)://", x = object@base_url)) {
    "@base_url has an invalid format"
  } else {
    TRUE
  }
})

# generics -----

#' @rdname acyRsaConnection-method
setGeneric("token", function(x) standardGeneric("token"))

#' @rdname acyRsaConnection-method
setGeneric("base_url", function(x) standardGeneric("base_url"))

#' @rdname acyRsaConnection-method
setGeneric("valid_until", function(x) standardGeneric("valid_until"))

#' @rdname acyRsaConnection-method
setGeneric("agent", function(x) standardGeneric("agent"))

#' @rdname acyRsaConnection-method
setGeneric("user_name", function(x) standardGeneric("user_name"))

# methods -----

#' @rdname acyRsaConnection-method
setMethod("token", "acyRsaConnection", function(x) x@token)

#' @rdname acyRsaConnection-method
setMethod("base_url", "acyRsaConnection", function(x) x@base_url)

#' @rdname acyRsaConnection-method
setMethod("valid_until", "acyRsaConnection", function(x) x@valid_until)

#' @rdname acyRsaConnection-method
setMethod("agent", "acyRsaConnection", function(x) x@agent)

#' @rdname acyRsaConnection-method
setMethod("user_name", "acyRsaConnection", function(x) x@user_name)

#' @rdname acyRsaConnection-method
setMethod("show", "acyRsaConnection", function(object){
  cat(is(object)[[1]], " Object\n",
      "   User:  ", object@user_name, "\n",
      "   Environment:  ", object@base_url, "\n",
      "   Valid until:  ", object@valid_until, "\n",
      sep = "")
})

#' @title Create rRofex Connection Object
#'
#' @description \code{rRofex_connection} creates a New Connection Object.
#'
#' @param token String. \strong{Mandatory} Obtained with \code{\link{acyrsa_login}}
#' @param base_url String. \strong{Mandatory} URL given by  \code{\link{acyrsa_login}} or known by the client.
#' @param user_name character. User Name
#'
#' @return S4 acyRsaConnection object.
#'
#' @note You can use accessors to get information about the Object by using:
#' \itemize{
#' \item \code{token(conn)}
#' \item \code{base_url(conn)}
#' \item \code{valid_until(conn)}
#' \item \code{agent(conn)}
#' \item \code{user_name(conn)}
#' }
acyrsa_connection <- function(token, base_url, user_name) {
  new(Class = "acyRsaConnection",
      token = token,
      base_url = base_url,
      valid_until = as.character(Sys.Date()),
      agent = paste0("acyRsa-", gsub(pattern = "(.+/)(.+?)(\\..+)", replacement = "\\2",x = base_url), "-", user_name),
      user_name = user_name)
}

