#' Getting Random values from Random.org
#' @title getRandom
#' @description Method to retrieve random values from Random.org API
#' @docType methods
#' @param REARandomObject an Object of class REARandom
#' @param simplifyVector A logical passed to the underlying call to jsonlite::fromJSON()
#' @param simplifyDataFrame A logical passed to the underlying call to jsonlite::fromJSON()
#' @param simplifyMatrix A logical passed to the underlying call to jsonlite::fromJSON()
#' @param flatten A logical passed to the underlying call to jsonlite::fromJSON()
#' @rdname REARandom-methods
#' @export getRandom
setGeneric("getRandom",
		   def = function(REARandomObject, simplifyVector, simplifyDataFrame,
		   			      simplifyMatrix, flatten) {
		   	standardGeneric("getRandom")
		   }, valueClass = "REARandom")

#' @title getRandom
#' @description Method used to create REARandom object that will be passed in API
#' call to Random.org's generateIntegers method.
#' @docType methods
#' @param REARandomObject an Object of class REARandom passed to Random.org API
#' to retrieve the specified type of random variates.
#' @param simplifyVector A logical passed to the underlying call to jsonlite::fromJSON()
#' @param simplifyDataFrame A logical passed to the underlying call to jsonlite::fromJSON()
#' @param simplifyMatrix A logical passed to the underlying call to jsonlite::fromJSON()
#' @param flatten A logical passed to the underlying call to jsonlite::fromJSON()
#' @examples \dontrun{
#'
#' # Create a new REARandom object to get a series of random integer values
#' myNewReaRandomObject <- reaRandom() %>%
#'						   setIntegers(n = 1000,
#'						   			   min = 1000000,
#'						   			   max = 9999999) %>%
#'						   getRandom(simplifyVector = FALSE,
#' 								  	 simplifyDataFrame = FALSE,
#' 								  	 simplifyMatrix = FALSE,
#' 								  	 flatten = FALSE)
#'
#' }
#' @family Retrieval Methods
#' @rdname getRandom
#' @import magrittr
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom RCurl httpPUT
#' @export getRandom
#'

setMethod(f = "getRandom",
		  signature("REARandom", "logical", "logical", "logical", "logical"),
		  definition = function(REARandomObject,
		  					    simplifyVector = TRUE,
		  					    simplifyDataFrame = FALSE,
		  					    simplifyMatrix = FALSE,
		  					    flatten = FALSE) {

		  # Create list object that will be serialized
		  objectList <- as.list(REARandomObject@jsonrpc,
		  					    REARandomObject@method,
		  					    REARandomObject@parameters,
		  					    REARandomObject@id) %>%
		  				jsonlite::toJSON(digits = 0)

		  # Submit request to site
		  payload <- RCurl::httpPUT(REARandomObject@requestHome, objectList) %>%
		  			 jsonlite::fromJSON(simplifyVector = simplifyVector,
		  			 				    simplifyDataFrame = simplifyDataFrame,
		  			 				    simplifyMatrix = simplifyMatrix,
		  			 				    flatten = flatten)

		  # Make sure the payload ID is the same as the ID passed in the request
		  if (payload[["id"]] != REARandomObject@id) {
		      stop("Requesting and Payload IDs do not match")
		  }

		  # Return the payload
		  return(payload)

}, valueClass = "REARandom") # End Method declaration

