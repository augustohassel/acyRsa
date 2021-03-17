#' @export acyrsa_login
#' @export acyrsa_horario_procesos
#' @export acyrsa_cuentas
#' @export acyrsa_margenes
#' @export acyrsa_garantias_integradas
#' @export acyrsa_activos_aceptados
#' @export acyrsa_cotizaciones
NULL

#' @include s4_object.R
NULL

# 1 :-- Authentication -----

#' @title Log-in Method
#'
#' @description It is use to Log-in and obtained a valid token tthat then should be used in all requests to the API.
#'
#' @param user String. \strong{Mandatory} User Name
#' @param pass String. \strong{Mandatory} Password
#' @param env String. \strong{Mandatory} Wich environment are you going to connect:
#' \itemize{
#' \item \strong{demoapi} - for testing
#' \item \strong{api} - for production
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
#' \item \code{login_date_time(conn)}
#' \item \code{agent(conn)}
#' \item \code{user_name(conn)}
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

  query <- POST(url = glue(base_url, "AuthToken/AuthToken"),
                query = list(nombreUsuario = user,
                             password = pass))

  if (status_code(query) != 200) {
    warn_for_status(query)
    NULL
  } else if (content(query)$Code == 200) {
    message(glue("Succesfully connected with acyRsa to {base_url}..."))

    invisible(acyrsa_connection(token = content(query)$Value,
                                base_url = base_url,
                                user_name = user))

  } else {
    message(glue("Something went wrong...

                 Error: {query}

                 Check function's arguments"))
    NULL
  }

}

# 2 :-- Administrative -----

#' @title Closing Proce
#'
#' @description List all processes and it time stamp.
#'
#' @param connection S4. \strong{Mandatory} Formal acyRsaConnection class object
#' @param date String. Date with format '\%Y-\%m-\%d'.
#' @param type Integer. Type of process. If 'NULL' (default) is all.
#' \itemize{
#' \item \strong{1} - 'Pasaje a definitivo'
#' \item \strong{2} - 'Calculo de margenes y garantias'
#' \item \strong{11} - 'Actualizacion de activos'
#' \item \strong{16} - 'Facturacion de comisiones, bonificaciones y servicios'
#' \item \strong{19} - 'Publicacion de indice accionario'
#' \item \strong{21} - 'Movimientos de custodia'
#' \item \strong{22} - 'Publicacion del porfolio'
#' \item \strong{23} - 'Publicacion de ajustes'
#' }
#'
#' @return A tibble.
acyrsa_horario_procesos <- function(connection, date = Sys.Date(), type = NULL) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "acyRsaConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'acyRsaConnection'.")
  if (as.Date(connection@valid_until) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!missing(date) & !.validate_fecha(date)) stop("Date must be given in the correct format")

  query <- GET(url = glue(connection@base_url, "PosTrade/ClosingProcesses"),
               query = list(
                 EntryDate = format.Date(date, "%Y%m%d"),
                 ProcessTypeCode = type
                 ),
               add_headers(.headers = c("Authorization" = glue("Token ", connection@token))),
               user_agent(connection@agent))

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else if  (content(query)$Status != "OK") {

    message(glue('Something wrong. See details:\n',
                 'Info: {message}\n',
                 '{description}',
                 message = content(query)$ErrorMessage,
                 description = content(query)$ErrorDescription))
    data <- NULL

  } else if (content(query)$Status == "OK") {

    data <- fromJSON(toJSON(content(query), null = "null", auto_unbox = TRUE, digits = NA))

    data <- data$Value %>%
      map(.x = ., ~ bind_rows(.x)) %>%
      unlist(recursive = F) %>%
      as_tibble() %>%
      rename_all(.tbl = ., .funs = list(~gsub(pattern = "(.+\\.)(.+)", replacement = "\\2", x = .))) %>%
      unnest(Entries) %>%
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
      mutate(
        DateTime = as.POSIXct(x = paste0(EntryDate, " ", EntryTime), tz = "America/Buenos_Aires")
      ) %>%
      select(-c(EntryDate, EntryTime))

  }

  return(data)

}

#' @title Depositary Account List
#'
#' @description Brings a list with information of the accounts owned by the broker.
#'
#' @param connection S4. \strong{Mandatory} Formal acyRsaConnection class object
#' @param market_account Logical. Type of market accounts.
#' \itemize{
#' \item \strong{TRUE} - Third party accounts
#' \item \strong{FALSE} - Owned accounts
#' }
#'
#' @return A tibble.
acyrsa_cuentas <- function(connection, market_account = TRUE) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "acyRsaConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'acyRsaConnection'.")
  if (as.Date(connection@valid_until) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  query <- GET(url = glue(connection@base_url, "PosTrade/DepositaryAccountList"),
               query = list(
                 MarketAccount = market_account
               ),
               add_headers(.headers = c("Authorization" = glue("Token ", connection@token))),
               user_agent(connection@agent))

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else if  (content(query)$Status != "OK") {

    message(glue('Something wrong. See details:\n',
                 'Info: {message}\n',
                 '{description}',
                 message = content(query)$ErrorMessage,
                 description = content(query)$ErrorDescription))
    data <- NULL

  } else if (content(query)$Status == "OK") {

    data <- fromJSON(toJSON(content(query), null = "null", auto_unbox = TRUE, digits = NA))

    data <- data$Value %>%
      as_tibble()

  }

  return(data)

}

#' @title Security List
#'
#' @description Method that provides tradable securities at Matba Rofex.
#'
#' @param connection S4. \strong{Mandatory} Formal acyRsaConnection class object
#' @param SecurityIDSource String. \strong{Mandatory} When MarketID = ROFX values can be M or 4, while MarketID = ACSA, then H or 4. Allowed values:
#' \itemize{
#' \item \strong{4} = ISIN Number
#' \item \strong{M} = Market
#' \item \strong{H} = Clearing House
#' }
#' @param SecurityType String. Allowed values:
#' \itemize{
#' \item \strong{FUT} = Future
#' \item \strong{OPT} = Option
#' \item \strong{FXSPOT} = FX Spot
#' \item \strong{CFD} = Contract for differences
#' \item \strong{CS} = Common Stock
#' \item \strong{SECLOAN} = Securities Loan
#' \item \strong{CD} = Certificate of deposit
#' \item \strong{GO} = General Obligation Bonds
#' \item \strong{FXNDF} = Non-deliverable forward
#' \item \strong{TD} = Time deposit
#' \item \strong{MLEG} = Combinated operations
#' }
#' @param SecurityExchange String. Allowed values:
#' \itemize{
#' \item \strong{ROFX} = Matba Rofex
#' \item \strong{XMAB} = MAE
#' \item \strong{XROX} = Mervaros
#' }
#' @param SecurityGroup String.
#' @param MarketID String. \strong{Mandatory} See allowed values:
#' \itemize{
#' \item \strong{ROFX} = Matba Rofex
#' \item \strong{ACSA} = ACSA
#' }
#'
#' @return A tibble.
acyrsa_instrumentos <- function(connection, SecurityIDSource, SecurityType, SecurityExchange, SecurityGroup, MarketID) {
  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "acyRsaConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'acyRsaConnection'.")
}

# 3 :-- Operative -----

#' @title Margin Requirement Report
#'
#' @description It is use to query the required margins for a certain date.
#'
#' @param connection S4. \strong{Mandatory} Formal acyRsaConnection class object
#' @param date String. Date with format '\%Y-\%m-\%d'.
#' @param viewDetails Logical. With or without breakdown.
#'
#' @return A tibble.
acyrsa_margenes <- function(connection, date = Sys.Date(), viewDetails = FALSE) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "acyRsaConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'acyRsaConnection'.")
  if (as.Date(connection@valid_until) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!missing(date) & !.validate_fecha(date)) stop("Date must be given in the correct format")

  query <- GET(
    url = glue(connection@base_url, "PosTrade/MarginRequirementReport"),
    query = list(
      date = format.Date(date, "%Y%m%d"),
      viewDetails = viewDetails
      ),
    add_headers(.headers = c("Authorization" = glue("Token ", connection@token))),
    user_agent(connection@agent)
    )

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else if  (content(query)$Status != "OK") {

    message(glue('Something wrong. See details:\n',
                 'Info: {message}\n',
                 '{description}',
                 message = content(query)$ErrorMessage,
                 description = content(query)$ErrorDescription))
    data <- NULL

  } else if (content(query)$Status == "OK") {

    data <- fromJSON(toJSON(content(query), null = "null", auto_unbox = TRUE, digits = NA))

    data <- if (is_empty(data$Value)) {
      message("There is currently no data to be displayed.")
      NULL
    } else {
      data$Value %>%
        unnest(Accounts) %>%
        mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
        unnest(SubAccounts) %>%
        mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
        unnest(References) %>%
        mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
        simplify_all() %>%
        as_tibble()
    }

  }

  return(data)

}

#' @title Integrated Guarantees
#'
#' @description It is use to query the list of integrated garantees from our clients in ACyRSA.
#'
#' @param connection S4. \strong{Mandatory} Formal acyRsaConnection class object
#' @param date String. Date with format '\%Y-\%m-\%d'.
#' @param cim Integer. \strong{Mandatory} Compensation Account Code
#' @param alyc Integer.\strong{Mandatory} Clearing Member Code
#'
#' @return A tibble.
acyrsa_garantias_integradas <- function(connection, cim, alyc, date = Sys.Date()) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "acyRsaConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'acyRsaConnection'.")
  if (as.Date(connection@valid_until) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (missing(cim) | missing(alyc)) stop("Compensation Account Code and Clearing Member Code are needed.")

  if (!missing(date) & !.validate_fecha(date)) stop("Date must be given in the correct format")

  query <- GET(url = glue(connection@base_url, "PosTrade/MT506"),
               query = list(date = format.Date(date, "%Y%m%d"),
                            cim = cim,
                            alyc = alyc),
               add_headers(.headers = c("Authorization" = glue("Token ", connection@token))),
               user_agent(connection@agent))

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else if  (content(query)$Status != "OK") {

    message(glue('Something wrong. See details:\n',
                 'Info: {message}\n',
                 '{description}',
                 message = content(query)$ErrorMessage,
                 description = content(query)$ErrorDescription))
    data <- NULL

  } else if (content(query)$Status == "OK") {

    data <- fromJSON(toJSON(content(query), null = "null", auto_unbox = TRUE, digits = NA))

    data <- data$Value %>%
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
      mutate_all(., unlist) %>%
      as_tibble() %>%
      separate(col = Party,
               into = c("CompensationAccountCode", "Comitente"), sep = "\\\\")

  }

  return(data)

}

#' @title Collateral List
#'
#' @description Brings a list of the products thar are currently accepted as collateral.
#'
#' @param connection S4. \strong{Mandatory} Formal acyRsaConnection class object
#' @param date String. Date with format '\%Y-\%m-\%d'.
#'
#' @return A tibble.
acyrsa_activos_aceptados <- function(connection, date = Sys.Date()) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "acyRsaConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'acyRsaConnection'.")
  if (as.Date(connection@valid_until) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!missing(date) & !.validate_fecha(date)) stop("Date must be given in the correct format")

  query <- GET(url = glue(connection@base_url, "PosTrade/CollateralList"),
               query = list(date = format.Date(date, "%Y%m%d")),
               add_headers(.headers = c("Authorization" = glue("Token ", connection@token))),
               user_agent(connection@agent))

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else if  (content(query)$Status != "OK") {

    message(glue('Something wrong. See details:\n',
                 'Info: {message}\n',
                 '{description}',
                 message = content(query)$ErrorMessage,
                 description = content(query)$ErrorDescription))
    data <- NULL

  } else if (content(query)$Status == "OK") {

    data <- fromJSON(toJSON(content(query), null = "null", auto_unbox = TRUE, digits = NA))

    data <- data$Value %>%
      mutate(PriceDate = as.Date(PriceDate),
             ExpirationDate = as.Date(PriceDate)
             ) %>%
      as_tibble()

  }

  return(data)

}

# 4 :-- Market Data -----

#' @title Market Data
#'
#' @description It is use to access Market Data from ACyRSA
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
#' @param Projected Boolean.
#'
#' @return A tibble.
#'
#' @examples
#' \dontrun{
#' acyrsa_cotizaciones(connection = conn, entry_type = 6, date = "2020-04-17", SecurityType = "FUT")
#' }
acyrsa_cotizaciones <- function(connection, entry_type, date, Symbol, CFICode, MarketID, MarketSegmentID, SecurityGroup, SecurityType, SecurityIDSource, Projected = TRUE) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "acyRsaConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'acyRsaConnection'.")
  if (as.Date(connection@valid_until) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (missing(entry_type)) stop("'entry_type' parameter is required.")
  if (some(entry_type, ~ !.x %in% c("2", "3", "5", "6", "B", "C", "D"))) stop("'entry_type' parameter is invalid. See documentation.")

  if (missing(date)) stop("'date' parameter is required.")
  if (!missing(date) & !.validate_fecha(date)) stop("Date must be given in the correct format.")

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
               add_headers(.headers = c("Authorization" = glue("Token ", connection@token))),
               user_agent(connection@agent))

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else if  (content(query)$Status != "OK") {

    message(glue('Something wrong. See details:\n',
                 'Info: {message}\n',
                 '{description}',
                 message = content(query)$ErrorMessage,
                 description = content(query)$ErrorDescription))
    data <- NULL

  } else if (content(query)$Status == "OK" && length(content(query)$Value) > 0) {

    data <- fromJSON(toJSON(content(query), digits = NA))

    data <- data$Value %>%
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
      unnest(Instrument) %>%
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
      unnest(MDFullGrp) %>%
      mutate_all(., ~ replace_na(data = ., replace = NA)) %>%
      mutate_all(., unlist) %>%
      as_tibble()

  } else if (content(query)$Status == "OK" && length(content(query)$Value) == 0) {

    message("No data available...")
    data <- NULL

  }

  return(data)

}
