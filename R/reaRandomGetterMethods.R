#' Getting Random values from Random.org
#' @title getRandom
#' @description Method to retrieve random values from Random.org API
#' @docType methods
#' @param simplifyVector A logical passed to the underlying call to jsonlite::fromJSON()
#' @param simplifyDataFrame A logical passed to the underlying call to jsonlite::fromJSON()
#' @param simplifyMatrix A logical passed to the underlying call to jsonlite::fromJSON()
#' @param flatten A logical passed to the underlying call to jsonlite::fromJSON()
#' @rdname REARandom-methods
#' @export getRandom
setGeneric("getRandom",
		   def = function(simplifyVector, simplifyDataFrame, simplifyMatrix,
		   			      flatten) {
		   	standardGeneric("getRandom")
		   }, valueClass = "REARandom")

#' @title getRandom
#' @description Method used to create REARandom object that will be passed in API
#' call to Random.org's generateIntegers method.
#' @docType methods
#' @param simplifyVector A logical passed to the underlying call to jsonlite::fromJSON()
#' @param simplifyDataFrame A logical passed to the underlying call to jsonlite::fromJSON()
#' @param simplifyMatrix A logical passed to the underlying call to jsonlite::fromJSON()
#' @param flatten A logical passed to the underlying call to jsonlite::fromJSON()
#' @examples \dontrun{
#'
#' 		# Create a new REARandom object
#' 		myNewReaRandomObject <- reaRandom()
#'
#' 		# Set object for retrieval of random integer values
#' 		myNewReaRandomObject.setIntegers(n = 1000, min = 1000000, max = 9999999)
#'
#' 		# Get the resulting request from Random.org
#' 		myNewReaRandomObject.getRandom(simplifyVector = FALSE,
#' 								 simplifyDataFrame = FALSE,
#' 								 simplifyMatrix = FALSE,
#' 								 flatten = FALSE)
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
		  signature("logical", "logical", "logical", "logical"),
		  definition = function(simplifyVector = TRUE, simplifyDataFrame = FALSE,
		  					  simplifyMatrix = FALSE, flatten = FALSE) {

		  # Create list object that will be serialized
		  objectList <- as.list(.Object@jsonrpc,
		  					    .Object@method,
		  					    .Object@parameters,
		  					    .Object@id) %>%
		  				jsonlite::toJSON(digits = 0)

		  # Submit request to site
		  payload <- RCurl::httpPUT(.Object@requestHome, objectList) %>%
		  			 jsonlite::fromJSON(simplifyVector = simplifyVector,
		  			 				    simplifyDataFrame = simplifyDataFrame,
		  			 				    simplifyMatrix = simplifyMatrix,
		  			 				    flatten = flatten)

		  # Return the payload
		  return(payload)

}, valueClass = "REARandom") # End Method declaration

