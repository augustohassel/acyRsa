#' @export acyRsaConnection
#' @export acyrsa_login
NULL

#' @include s4_object.R
#' NULL

#' @title New acyRsa Connection
#'
#' @description \code{acyRsaConnection} creates a New Connection Object. It must be created manually when using de API with the token authentication.
#'
#' @param token String. Obtained with \code{\link{acyrsa_login}}
#' @param base_url String. URL given by  \code{\link{acyrsa_login}} or known by the client.
#'
#' @return Creates a 'acyRsaConnection' S4 Object
#'
#' @examples
#' \dontrun{conn <- acyRsaConnection(token = "1234", base_url = "https://api.anywhereportfolio.com.ar/")}
acyRsaConnection <- function(token, base_url) {
  new("acyRsaConnection", token = token, base_url = base_url)
}

#' @title Log-In
#'
#' @description \code{acyrsa_login} method it's used to Log-In and to obtained a valid token to be used in all requests to the API.
#'
#' @param user String. User Name
#' @param pass String. Password
#' @param env String. Wich environment are you going to connect:
#'\itemize{
#'\item testing: 'demoapi'
#'\item production: 'api'
#'}
#'
#' @return Creates a 'acyRsaConnection' S4 Object with a token and a base_url. The token is only valid for a day.
#'
#' @examples
#' \dontrun{conn <- acyrsa_login(user = "asd", pass = "xxx", env = "api")}
acyrsa_login <- function(user, pass, env) {
  if (missing(user) | missing(pass)) stop("Username and Password are needed.")
  if (missing(env)) stop("Environment is needed.")
  if (!env %in% c("api", "demoapi")) stop("Environrment is invalid.")

  # Base URL
  base_url <- glue('https://{env}.anywhereportfolio.com.ar/')

  url <- glue(base_url, "AuthToken/AuthToken")

  query <- POST(url = url,
                query = list(nombreUsuario = user,
                             password = pass))


  if (status_code(query) != 200) {
    warn_for_status(query)
  } else if (content(query)$Code == 200) {
    message_for_status(query)

    message("\nConnected to API BO...")

    invisible(acyRsaConnection(token = content(query)$Value, base_url = base_url))

  } else {
    warning("Something went wrong...")
  }

}
