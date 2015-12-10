#' Defines the base object used for API requests
#' @title REARandom
#' @rdname REARandom-class
#' @author Billy Buchanan
#' @docType class
#' @aliases reaRandom-class
#' @details Generates a new S4 object with methods to access random numbers
#' provided by Random.org's API
#' @slot requestHome The URL where the API requests will be submitted
#' @slot apiKey The API Key for Random.org API Access
#' @slot jsonrpc The version of the JSONRPC protocol
#' @slot method The API method call to issue
#' @slot parameters The parameters to be passed to the API Method
#' @slot id An ID variable used to match requests with payloads
#' @import methods
#' @export REARandom
#' @exportClass REARandom
#'

REARandom <- setClass(Class = "REARandom",
					  representation(requestHome = "character",
					  			     apiKey = "character",
					  			     jsonrpc = "character",
					  			     method = "character",
					  			     parameters = "character",
					  			     id = "numeric"))

# Validation function for Class REARandom
validREARandom <- function(object) {
	if (object@requestHome != "https://api.random.org/json-rpc/1/invoke") FALSE
	if (object@jsonrpc != "2.0") FALSE
	if (is.null(object@apiKey) || is.na(object@apiKey)) FALSE
	else TRUE
}

# Sets the validation function for object initialization
setValidity("REARandom", validREARandom)

# Initialization method for REARandom class
setMethod("initialize", "REARandom",
		  function(.Object) {
		  	.Object@requestHome = "https://api.random.org/json-rpc/1/invoke"
		  	.Object@jsonrpc = "2.0"
		  	return(.Object)
		  }, valueClass = "REARandom"
)


#' Generic method declaration for API Key setter
#' @title setApiKey
#' @description Method used to set the ApiKey member of the REARandom class
#' @param REARandomObject an Object of class REARandom
#' @param api A character string with the API key, a file name containing the API key, or NULL value
#' @rdname setApiKey
#' @family REARandom Constructor Methods
#' @docType methods
#'

setGeneric("setApiKey",
		   def = function(REARandomObject, api) {
			standardGeneric("setApiKey")
			}, valueClass = "REARandom")

#' Method used to allow user to pass a file name/string with the API key
#' @title setApiKey
#' @family REARandom Constructor Methods
#' @rdname setApiKey
#' @export setApiKey
#'

setMethod(f = "setApiKey", signature("REARandom", "character"),
		  definition = function(REARandomObject, api = "character") {
		  	e <- simpleError(message = NULL, call = api)
		  	warn <- simpleWarning(message = NULL, call = api)
		  	apiKey <- tryCatch(scan(api, what = "character"),
		  		               error = function(e) e,
		  					   warning = function(warn) warn)
		  	return(apiKey)
		  	REARandomObject@apiKey <- apiKey

		  # Make sure method returns the REARandom class object
		  return(REARandomObject)

}, valueClass = "REARandom")

#' @family REARandom Constructor Methods
#' @rdname setApiKey
#' @export setApiKey
#'

setMethod(f = "setApiKey", signature("REARandom", "missing"),
		  definition = function(REARandomObject, api = NULL) {
		  	REARandomObject@apiKey = getOption("reaRandomKey")

		  # Make sure method returns the REARandom class object
		  return(REARandomObject)

}, valueClass = "REARandom")

#' Function to construct new REARandom class objects
#' @title reaRandom
#' @description Function used to create new REARandom class objects
#' @param api Takes a single character argument with either a file containing
#' the api key for Random.org, the key itself, or a NULL value.
#' @return Returns an object of class REARandom
#' @docType methods
#' @examples \dontrun{
#'
#' # Create a new REARandom object in x with made up API key passed as an
#' # argument to the function
#' x <- reaRandom(api = "ABCD123456789abcd")
#'
#' # Same as above but assuming the API key is stored in a file on the local
#' # system
#' x <- reaRandom(api = "~/.randomOrgApiKey")
#'
#' # Same as above assuming the API key is stored as an option named
#' # "reaRandomKey"
#' x <- reaRandom()
#'
#' }
#' @export reaRandom
#'

reaRandom <- function(api = NULL) {
	reaRandomObject <-  new("REARandom") %>%
						setApiKey(api)
	validObject(reaRandomObject)
	return(reaRandomObject)
}
