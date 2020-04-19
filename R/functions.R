#' @export acyrsa_connection
#' @export acyrsa_login
#' @export acyrsa_margenes
#' @export acyrsa_garantias_integradas
NULL

#' @include s4_object.R
#' NULL

#' @title Create acyRsa Connection Object
#'
#' @description \code{acyrsa_connection} creates a New Connection Object. It must be created manually when using de API with the token authentication.
#'
#' @param token String. Obtained with \code{\link{acyrsa_login}}
#' @param base_url String. URL given by  \code{\link{acyrsa_login}} or known by the client.
#'
#' @return Creates a 'acyrsa_connection' S4 Object
#'
#' @note You can use accesors to get information about the Object by using:
#' \itemize{
#' \item \code{token(conn)}
#' \item \code{base_url(conn)}
#' }
#'
#' @examples
#' \dontrun{conn <- acyrsa_connection(token = "1234", base_url = "https://api.anywhereportfolio.com.ar/")}
acyrsa_connection <- function(token, base_url) {
  new("acyRsaConnection", token = token, base_url = base_url)
}

#' @title Log-In Method
#'
#' @description \code{acyrsa_login} method it's used to Log-In and to obtained a valid token to be used in all requests to the API.
#'
#' @param user String. User Name
#' @param pass String. Password
#' @param env String. Wich environment are you going to connect:
#' \itemize{
#' \item testing: 'demoapi'
#' \item production: 'api'
#' }
#'
#' @return Creates a 'acyRsaConnection' S4 Object with a token and a base_url. The token is only valid for a day.
#'
#' @note You can use accesors to get information about the Object by using:
#' \itemize{
#' \item \code{token(conn)}
#' \item \code{base_url(conn)}
#' }
#'
#' @examples
#' \dontrun{conn <- acyrsa_login(user = "asd", pass = "xxx", env = "api")}
acyrsa_login <- function(user, pass, env) {
  if (missing(user) | missing(pass)) stop("Username and Password are needed.")
  if (missing(env)) stop("Environment is needed.")
  if (!env %in% c("api", "demoapi")) stop("Environrment is invalid. See documentation.")

  # Base URL
  base_url <- glue('https://{env}.anywhereportfolio.com.ar/')

  url <- glue(base_url, "AuthToken/AuthToken")

  query <- POST(url = url,
                query = list(nombreUsuario = user,
                             password = pass))

  if (status_code(query) != 200) {
    warn_for_status(query)
    NULL
  } else if (content(query)$Code == 200) {

    message_for_status(query)
    message("\nSuccesfully connected to API BO...")
    invisible(acyrsa_connection(token = content(query)$Value, base_url = base_url))

  } else {
    warning("Something went wrong...")
    NULL
  }

}


#' @title Margin Requirement Report
#'
#' @description \code{acsa_margenes} method it's used to query the required margins for a certain date.
#'
#' @param connection S4. Formal acyRsaConnection class object
#' @param date String. Date with format '\%Y-\%m-\%d'.
#'
#' @return Data Frame
acyrsa_margenes <- function(connection, date = Sys.Date()) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "acyRsaConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'acyRsaConnection'.")
  if (as.Date(connection@valid_until) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!missing(date) & !.validate_date(date)) stop("Date mus be given in the correct format")

  query <- GET(url = glue(connection@base_url, "PosTrade/MarginRequirementReport"),
               query = list(date = format.Date(date, "%Y%m%d")),
               add_headers(.headers = c("Authorization" = glue("Token ", connection@token))))

  if (status_code(query) != 200) {
    warn_for_status(query)
    NULL
  } else if (content(query)$Code == 200) {
    message_for_status(query)

    data <- fromJSON(toJSON(content(query)$Value))

    data <- data %>%
      unnest(Accounts) %>%
      unnest(SubAccounts) %>%
      unnest(References) %>%
      simplify_all() %>%
      as_tibble()

    return(data)

  } else {
    warning("Something went wrong...")
    NULL
  }

}

#' @title Integrated Guarantees
#'
#' @description \code{acyrsa_garantias_integradas} method it's used to query the list of integrated garantees from our clients in ACyRSA.
#'
#' @param connection S4. Formal acyRsaConnection class object
#' @param date String. Date with format '\%Y-\%m-\%d'.
#' @param cim Integer. Compensation Account Code
#' @param alyc Integer.Clearing Member Code
#'
#' @return Data Frame
acyrsa_garantias_integradas <- function(connection, cim, alyc, date = Sys.Date()) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "acyRsaConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'acyRsaConnection'.")
  if (as.Date(connection@valid_until) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (missing(cim) | missing(alyc)) stop("Compensation Account Code and Clearing Member Code are needed.")

  if (!missing(date) & !.validate_date(date)) stop("Date mus be given in the correct format")

  query <- GET(url = glue(connection@base_url, "PosTrade/MT506"),
               query = list(date = format.Date(date, "%Y%m%d"),
                            cim = cim,
                            alyc = alyc),
               add_headers(.headers = c("Authorization" = glue("Token ", connection@token))))

  if (status_code(query) != 200) {
    warn_for_status(query)
    NULL
  } else if (content(query)$Code == 200) {
    message_for_status(query)

    data <- fromJSON(toJSON(content(query)$Value))

    data <- data %>%
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
      mutate_all(., unlist) %>% as_tibble() %>%
      separate(col = Party,
               into = c("CompensationAccountCode", "Comitente"), sep = "\\\\")

  } else {
    warning("Something went wrong...")
    NULL
  }

}
