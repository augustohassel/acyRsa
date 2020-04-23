#' @export acyrsa_connection
#' @export acyrsa_login
#' @export acyrsa_margenes
#' @export acyrsa_garantias_integradas
#' @export acyrsa_cotizaciones
NULL

#' @include s4_object.R
#' NULL

#' @title cyRsaConnection Object
#'
#' @description `acyrsa_connection()` creates a New Connection Object. It should be created manually when saving the token outside the application.
#'
#' @param token String. \strong{Mandatory} Obtained with \code{\link{acyrsa_login}}
#' @param base_url String. \strong{Mandatory} URL given to \code{\link{acyrsa_login}} to initiate the connection.
#'
#' @return Creates an 'acyRsaConnection' S4 Object
#'
#' @section Accesors:
#' You can use accesors to get information about the Object by using:
#' \itemize{
#' \item \code{token(conn)}
#' \item \code{base_url(conn)}
#' \item \code{valid_until(conn)}
#' }
#'
#' @family connection functions
#'
#' @examples
#' \dontrun{
#' conn <- acyrsa_connection(token = "1234", base_url = "https://api.anywhereportfolio.com.ar/")
#' }
acyrsa_connection <- function(token, base_url) {
  new("acyRsaConnection", token = token, base_url = base_url, valid_until = as.character(Sys.Date()))
}

#' @title Log-in Method
#'
#' @description `acyrsa_login()` it's used to Log-in and obtained a valid token tthat then should be used in all requests to the API.
#'
#' @param user String. \strong{Mandatory} User Name
#' @param pass String. \strong{Mandatory} Password
#' @param env String. \strong{Mandatory} Wich environment are you going to connect:
#' \itemize{
#' \item 'demoapi' for testing
#' \item 'api' for production
#' }
#'
#' @return Creates an 'acyRsaConnection' S4 Object with a token and a base_url.
#'
#' @note The token is valid only for a day.
#'
#' @section Accesors:
#' You can use accesors to get information about the Object by using:
#' \itemize{
#' \item \code{token(conn)}
#' \item \code{base_url(conn)}
#' \item \code{valid_until(conn)}
#' }
#'
#' @family connection functions
#'
#' @examples
#' \dontrun{
#' conn <- acyrsa_login(user = "asd", pass = "xxx", env = "api")
#' }
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
#' @description `acyrsa_margenes()` it's used to query the required margins for a certain date.
#'
#' @param connection S4. \strong{Mandatory} Formal acyRsaConnection class object
#' @param date String. Date with format '\%Y-\%m-\%d'.
#'
#' @return Data Frame
acyrsa_margenes <- function(connection, date = Sys.Date()) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "acyRsaConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'acyRsaConnection'.")
  if (as.Date(connection@valid_until) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!missing(date) & !.validate_date(date)) stop("Date must be given in the correct format")

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
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
      unnest(SubAccounts) %>%
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
      unnest(References) %>%
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
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
#' @description `acyrsa_garantias_integradas()` it's used to query the list of integrated garantees from our clients in ACyRSA.
#'
#' @param connection S4. \strong{Mandatory} Formal acyRsaConnection class object
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

  if (!missing(date) & !.validate_date(date)) stop("Date must be given in the correct format")

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

    return(data)

  } else {
    warning("Something went wrong...")
    NULL
  }

}

#' @title Market Data
#'
#' @description `acyrsa_cotizaciones()` it's used to access Market Data from ACyRSA
#'
#' @param connection S4. \strong{Mandatory} Formal acyRsaConnection class object
#' @param entry_type String. \strong{Mandatory} Vector of one or many values. See allowed values:
#' \itemize{
#' \item \strong{2} = Tick By Tick
#' \item \strong{3} = Index Value
#' \item \strong{5} = Clossing Prices
#' \item \strong{6} = Settlement Price
#' \item \strong{B} = Trade Volume
#' \item \strong{C} = Open Interest
#' \item \strong{D} = Composite Underlying Price
#' }
#' @param date String. \strong{Mandatory} Clearing Business Day with format '\%Y-\%m-\%d'.
#' @param Symbol String. Contract Symbol.
#' @param CFICode String.
#' @param MarketID String. See allowed values:
#' \itemize{
#' \item \strong{ROFX} = Matba Rofex
#' \item \strong{XMAB} = MAE
#' }
#' @param MarketSegmentID String. See allowed values:
#' \itemize{
#' \item \strong{DDF} = Financial
#' \item \strong{DDA} = Agricultural
#' \item \strong{DUAL} = Others
#' }
#' @param SecurityGroup String.
#' @param SecurityType String. See allowed values:
#' \itemize{
#' \item \strong{FUT} = Future
#' \item \strong{OPT} = Option
#' \item \strong{FXSPOT} = FX Spot
#' \item \strong{CFD} = Contract for differences
#' \item \strong{CS} = Common Stock
#' \item \strong{MF} = Mutual Fund
#' \item \strong{SECLOAN} = Security Loan
#' \item \strong{CD} = Certificate of Deposit
#' \item \strong{GO} = General Obligation Bonds
#' \item \strong{FXNDF} = Non-deliverable forward
#' \item \strong{TD} = Time Deposit
#' }
#' @param SecurityIDSource String. See allowed values:
#' \itemize{
#' \item \strong{4} = ISIN Number
#' }
#'
#' @return Data Frame
#'
#' @examples
#' \dontrun{
#' acyrsa_cotizaciones(connection = conn, entry_type = 6, date = "2020-04-17", SecurityType = "FUT")
#' }
acyrsa_cotizaciones <- function(connection, entry_type, date, Symbol, CFICode, MarketID, MarketSegmentID, SecurityGroup, SecurityType, SecurityIDSource) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "acyRsaConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'acyRsaConnection'.")
  if (as.Date(connection@valid_until) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (missing(entry_type)) stop("'entry_type' parameter is required.")
  if (some(entry_type, ~ !.x %in% c("2", "3", "5", "6", "B", "C", "D"))) stop("'entry_type' parameter is invalid. See documentation.")

  if (missing(date)) stop("'date' parameter is required.")
  if (!missing(date) & !.validate_date(date)) stop("Date must be given in the correct format.")

  if (!missing(MarketID) && some(MarketID, ~ !.x %in% c("ROFX", "XMAB"))) stop("'MarketID' parameter is invalid. See documentation.")

  if (!missing(MarketSegmentID) && some(MarketSegmentID, ~ !.x %in% c("DDF", "DDA", "DUAL"))) stop("'MarketSegmentID' parameter is invalid. See documentation.")

  if (!missing(SecurityType) && some(SecurityType, ~ !.x %in% c("FUT", "OPT", "FXSPOT", "CFD", "CS", "MF", "SECLOAN", "CD", "GO", "FXNDF", "TD"))) stop("'SecurityType' parameter is invalid. See documentation.")

  if (!missing(SecurityIDSource) && some(SecurityIDSource, ~ !.x %in% c("4"))) stop("'SecurityIDSource' parameter is invalid. See documentation.")

  query <- GET(url = glue(connection@base_url, "PosTrade/MarketData"),
               query = unlist(
                 list(
                   list(mdEntryType = glue_collapse(entry_type, sep = ","),
                        ClearingBusinessDate = format.Date(date, "%Y%m%d")),
                   as.list(match.call())[-c(1:4)]
                   ),
                 recursive = F),
               add_headers(.headers = c("Authorization" = glue("Token ", connection@token))))

  if (status_code(query) != 200) {
    warn_for_status(query)
    NULL
  } else if (content(query)$Code == 200 & length(content(query)$Value) > 0) {

    message_for_status(query)

    data <- fromJSON(toJSON(content(query)$Value))

    data <- data %>%
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
      unnest(Instrument) %>%
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
      unnest(MDFullGrp) %>%
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
      mutate_all(., unlist) %>%
      as_tibble()

    return(data)

  } else if (content(query)$Code == 200 & length(content(query)$Value) == 0) {
    warning("No data available...")
    NULL
  } else {
    warning("Something went wrong...")
    NULL
  }

}
