#' Method to create object to pass to the generateIntegers method of the
#' Random.org API
#' @title Random Integers
#' @description Method to create REARandom object used for Random.org method
#' call to get random integers.
#' @param id ID Used for retrieval and matching of API call request and payload; default is 42.
#' @param n Number of random values to generate; default is 1.
#' @param min The minimum value integer to use for the request; default is 1,000,000.
#' @param max The maximum value integer to use for the request; default is 9,999,999.
#' @param replacement Whether the values should be sampled with or without replacement
#' from the distribution.
#' @param base Which base value should the numbers be reported in;
#' 	defaults to base 10 (decimal).  A value of 2 would provide binary values,
#' 	a value of 8 would return octal values, and value of 16 would return
#' 	hexadecimal values.
#' @docType methods
#' @rdname REARandom-methods
#' @export setIntegers
setGeneric("setIntegers",
		   def = function(id, n, min, max, replacement, base) {
		   	standardGeneric("setIntegers")
		   }, valueClass = "REARandom")

#' @title setIntegers
#' @description Method used to create REARandom object that will be passed in API
#' call to Random.org's generateIntegers method.
#' @param id ID Used for retrieval and matching of API call request and payload; default is 42.
#' @param n Number of random values to generate; default is 1.
#' @param min The minimum value integer to use for the request; default is 1.
#' @param max The maximum value integer to use for the request; default is 999.
#' @param replacement Whether the values should be sampled with or without replacement
#' from the distribution.
#' @param base Which base value should the numbers be reported in;
#' 	defaults to base 10 (decimal).  A value of 2 would provide binary values,
#' 	a value of 8 would return octal values, and value of 16 would return
#' 	hexadecimal values.
#' @docType methods
#' @examples \dontrun{
#'
#' # Create a new REARandom object
#' myNewReaRandomObject <- reaRandom()
#'
#' # Set object for retrieval of random integer values
#' myNewReaRandomObject.setIntegers(id = 12,
#' 									n = 1000,
#' 									min = 1000,
#' 									max = 9999,
#' 									replacement = TRUE,
#' 									base = 8)
#'
#' }
#' @family REARandom Constructor Methods
#' @rdname setIntegers
#' @export setIntegers
#'

setMethod(f = "setIntegers",
		  signature("numeric", "numeric", "numeric", "numeric", "logical", "numeric"),
		  definition = function(id = 42, n = 1, min = 1, max = 999,
		  					  replacement = FALSE, base = 10) {

		  # Check number of values requested
		  if (!(n %in% c(1:1e4))) stop("Parameter n must be in [1, 1e4]")

		  # Check minimum and maximum value parameters
		  if (!(min %in% c(-1e9:1e9)) || !(max %in% c(-1e9:1e9))) {
		  	  stop("Minimum or Maximum value outside of [-1e9, 1e9]")
		  }

		  # Set the method slot
		  REARandom@method <- "generateIntegers"

		  # Set the ID slot
		  REARandom@id <- id

		  # Set API Method parameters
		  REARandom@parameters <- as.list("apiKey" = .Object@apiKey,
		  								"n" = n,
		  								"min" = min,
		  								"max" = max,
		  								"replacement" = replacement,
		  								"base" = base)

}, valueClass = "REARandom") # End Method declaration

#' Method to create object used for generateDecimalFractions API Method
#' @title Get Fractional Random Values
#' @description Creates object used to Random.org's generateDecimalFractions API method
#' @param decimalPlaces The number of decimal places to return.  Must be in [1, 20]; defaults to 1.
#' @export setDecimals
#' @docType methods
#' @rdname REARandom-methods
#'

setGeneric("setDecimals",
		   def = function(id, n, decimalPlaces, replacement) {
		   	standardGeneric("setDecimals")
		   }, valueClass = "REARandom")

#' @title setDecimals
#' @description Method used to create REARandom object that will be passed in API
#' call to Random.org's generateDecimalFractions method.
#' @param id An ID number used to match response to request. Defaults to 42.
#' @param n The number of random values to return. Must be in [1, 1e4]; defaults to 1.
#' @param decimalPlaces The number of decimal places to return.  Must be in [1, 20]; defaults to 1.
#' @param replacement Should numbers be sampled with or without replacement.
#' 			Default is FALSE (e.g., unique values).
#' @docType methods
#' @examples \dontrun{
#'
#' # Create a new REARandom object
#' myNewReaRandomObject <- reaRandom()
#'
#' # Set object for retrieval of fractional random variables
#' myNewReaRandomObject.setDecimals(id = 42,
#' 								 	n = 300,
#' 								 	decimalPlaces = 5,
#' 								 	replacement = TRUE)
#'
#' }
#' @family REARandom Constructor Methods
#' @rdname setDecimals
#' @export setDecimals
#'

setMethod(f = "setDecimals",
		  signature("numeric", "numeric", "numeric", "logical"),
		  definition = function(id = 42, n = 1, decimalPlaces = 1,
		  					  replacement = FALSE) {

		  # Check number of random numbers requested
		  if (!(n %in% c(1:1e4))) stop("Parameter n must be in [1, 1e4]")

		  # Check number of values beyond the decimal within allowed range
		  if (!(decimalPlaces %in% c(1:20))) stop("Parameter decimalPlaces must be in [1, 20]")

		  # Set the method slot
		  REARandom@method <- "generateDecimalFractions"

		  # Set the ID slot
		  REARandom@id <- id

		  # Set the API method parameters
		  REARandom@parameters <- as.list("apiKey" = .Object@apiKey,
		  								  "n" = n,
		  								  "decimalPlaces" = decimalPlaces,
		    			                  "replacement" = replacement)

}, valueClass = "REARandom") # End of Method declaration

#' Method to create object used for generateGaussians API Method
#' @title Get Gaussian Random Normal Variables from Random.org
#' @description Creates object used to Random.org's generateGaussians API method
#' @param mean Value of the mean for the distribution from which to draw random numbers.
#' 			Must be in [-1e6, 1e6]; defaults to 0.
#' @param standardDeviation Value for the standard deviation of the distribution.
#' 			Must be in [-1e6, 1e6]; defaults to 1.
#' @param significantDigits The number of digits beyond the decimal to retain.
#' 			Must be in [2, 20]; defaults to 2.
#' @export setNormal
#' @docType methods
#' @rdname REARandom-methods
#'

setGeneric("setNormal",
		   def = function(id, n, mean, standardDeviation, significantDigits) {
		   	standardGeneric("setNormal")
		   }, valueClass = "REARandom")

#' @title setNormal
#' @description Method used to create REARandom object that will be passed in API
#' call to Random.org's generateGaussians method.
#' @docType methods
#' @param id An ID number used to match response to request. Defaults to 42.
#' @param n The number of random values to return. Must be in [1, 1e4]; defaults to 1.
#' @param mean Value of the mean for the distribution from which to draw random numbers.
#' 			Must be in [-1e6, 1e6]; defaults to 0.
#' @param standardDeviation Value for the standard deviation of the distribution.
#' 			Must be in [-1e6, 1e6]; defaults to 1.
#' @param significantDigits The number of digits beyond the decimal to retain.
#' 			Must be in [2, 20]; defaults to 2.
#' @examples \dontrun{
#'
#' # Create a new REARandom object
#' myNewReaRandomObject <- reaRandom()
#'
#' # Set object for retrieval of Gaussian Random variables with $\mu$ = 100
#' # \& $\sigma$ = 15.
#' myNewReaRandomObject.setNormal(id = 37,
#' 								  mean = 100,
#' 								  standardDeviation = 15,
#' 								  significantDigits = 10)
#'
#' }
#' @family REARandom Constructor Methods
#' @rdname setNormal
#' @export setNormal
#'

setMethod(f = "setNormal",
		  signature("numeric", "numeric", "numeric", "numeric", "numeric"),
		  definition = function(id = 42, n = 1, mean = 0,
		  					  standardDeviation = 1, significantDigits = 2) {

		  # Check number of random numbers requested
		  if (!(n %in% c(1:1e4))) stop("Parameter n must be in c(1:1e4)")

		  # Check the value of mean/standardDeviation in acceptable range
		  if (!(mean %in% c(-1e6:1e6)) || !(standardDeviation %in% c(-1e6:1e6))) {
		  	  stop("Mean parameter outside of [-1e6, 1e6]")
		  }

		  # Check number of values beyond the decimal within allowed range
		  if (!(significantDigits %in% c(2:20))) stop("Parameter decimalPlaces must be in [2, 20]")

		  # Set the method slot
		  REARandom@method <- "generateGaussians"

		  # Set the ID slot
		  REARandom@id <- id

		  # Set the API method parameters
		  REARandom@parameters <- as.list("apiKey" = .Object@apiKey,
		  								  "n" = n,
		  								  "mean" = mean,
		    			                  "standardDeviation" = standardDeviation,
		  								  "significantDigits" = significantDigits)

}, valueClass = "REARandom") # End of Method declaration

#' Method to create object used for generateStrings API Method
#' @title Get Random Strings from Random.org
#' @description Creates object used to Random.org's generateStrings API method
#' @param length The number of characters to include in each random string (must be in [1, 20])
#' @param characters The set of characters from which to generate random strings.
#' 			Must have length <= 80.
#' @export setStrings
#' @docType methods
#' @rdname REARandom-methods
#'

setGeneric("setStrings",
		   def = function(id, n, length, characters, replacement) {
		   	standardGeneric("setStrings")
		   }, valueClass = "REARandom")

#' @title setStrings
#' @description Method used to create REARandom object that will be passed in API
#' call to Random.org's generateStrings method.
#' @param id An ID number used to match response to request. Defaults to 42.
#' @param n The number of random values to return must be in [1, 1e4]
#' @param length The number of characters to include in each random string (must be in [1, 20])
#' @param characters The set of characters from which to generate random strings.
#' 			Must have length <= 80.
#' @param replacement Should numbers be sampled with or without replacement.
#' 			Default is FALSE (e.g., unique values).
#' @docType methods
#' @examples \dontrun{
#'
#' # Create a new REARandom object
#' myNewReaRandomObject <- reaRandom()
#'
#' # Create object with characters to use in method call
#' myChars <- c(letters, LETTERS, c(0:9), "!", "@", "#", "$", "%", "^",
#' 					"&", "-", ",", "?", "|", ":", ";", "<", ">", ",", ".", "_")
#'
#' # Set object for retrieval of Binary Large OBjects that are 4MB in Size
#' # and serialized as Hexadecimal values
#' myNewReaRandomObject.setStrings(id = 37,
#' 								   n = 12,
#' 								   length = 4096,
#' 								   characters = myChars,
#' 								   replacement = FALSE)
#'
#' }
#' @family REARandom Constructor Methods
#' @rdname setStrings
#' @export setStrings
#'

setMethod(f = "setStrings",
		  signature("numeric", "numeric", "numeric", "character", "logical"),
		  definition = function(id = 42, n = 1, length = 1,
		  					  characters = c(letters, LETTERS, c(0:9),
		  					  			   "!", "@", "#", "$", "%", "^", "&",
		  					  			   "-", ",", "?", "|", ":", ";", "<",
		  					  			   ">", ",", ".", "_"),
		  					  replacement = FALSE) {

		  # Check number of random numbers requested
		  if (!(n %in% c(1:1e4))) stop("Parameter n must be in c(1:1e4)")

		  # Check the value of mean/standardDeviation in acceptable range
		  if (!(length %in% c(1:20))) stop("Length parameter outside of [1, 20]")

		  # Check number of values beyond the decimal within allowed range
		  if (!(nchar(characters) %in% c(1:80))) {
		  	stop(paste("Characters parameter must include 1-80 characters",
                       "from which to construct the random string."))
		  }

		  # Set the method slot
		  REARandom@method <- "generateStrings"

		  # Set the ID slot
		  REARandom@id <- id

		  # Set the API method parameters
		  REARandom@parameters <- as.list("apiKey" = .Object@apiKey,
		  								  "n" = n,
		  								  "length" = length,
		    			                  "characters" = characters,
		  								  "replacement" = replacement)

}, valueClass = "REARandom") # End of Method declaration

#' Method to create object used for generateUUIDs API Method
#' @title Get Uniform Unique IDentifiers from Random.org
#' @description Creates object used to Random.org's generateUUIDs API method
#' @export setUniqueID
#' @docType methods
#' @rdname REARandom-methods
#'

setGeneric("setUniqueID",
		   def = function(id, n) {
		   	standardGeneric("setUniqueID")
		   }, valueClass = "REARandom")

#' @title setUniqueID
#' @description Method used to create REARandom object that will be passed in API
#' call to Random.org's generateUUIDs method.
#' @param id An ID number used to match response to request. Defaults to 42.
#' @param n The number of Unique Uniform IDentifiers to return (must be in [1, 1e3])
#' @docType methods
#' @examples \dontrun{
#'
#' # Create a new REARandom object
#' myNewReaRandomObject <- reaRandom()
#'
#' # Set object to retrieve a series of Uniform Unique IDentifiers
#' myNewReaRandomObject.setUniqueID(id = 53, n = 500)
#'
#' }
#' @family REARandom Constructor Methods
#' @rdname setUniqueID
#' @export setUniqueID
#'

setMethod(f = "setUniqueID",
		  signature("numeric", "numeric"),
		  definition = function(id = 42, n = 1) {

		  # Check number of random numbers requested
		  if (!(n %in% c(1:1e3))) stop("Parameter n must be in [1, 1e3]")

		  # Set the method slot
		  REARandom@method <- "generateUUIDs"

		  # Set the ID slot
		  REARandom@id <- id

		  # Set the API method parameters
		  REARandom@parameters <- as.list("apiKey" = .Object@apiKey,
		  								  "n" = n)

}, valueClass = "REARandom") # End of Method declaration

#' Method to create object used for generateBlobs API Method
#' @title Get Binary Large Objects from Random.org
#' @description Creates object used to Random.org's generateBlobs API method
#' @param size The size in bits of the objects to return (must be divisible by 8)
#' @param format Either 'base64' or 'hex' to specify how the BLOBs are serialized
#' @export setBLOBs
#' @docType methods
#' @rdname REARandom-methods
#'

setGeneric("setBLOBs",
		   def = function(id, n, size, format) {
		   	standardGeneric("setBLOBs")
		   }, valueClass = "REARandom")

#' @title setBLOBs
#' @description Method used to create REARandom object that will be passed in API
#' call to Random.org's generateBlobs method.
#' @param id An ID number used to match response to request. Defaults to 42.
#' @param n The number of random values to return must be in [1, 100]
#' @param size The size in bits of the objects to return (must be divisible by 8)
#' @param format Either 'base64' or 'hex' to specify how the BLOBs are serialized
#' @docType methods
#' @examples \dontrun{
#'
#' # Create a new REARandom object
#' myNewReaRandomObject <- reaRandom()
#'
#' # Set object for retrieval of Binary Large OBjects that are 4MB in Size
#' # and serialized as Hexadecimal values
#' myNewReaRandomObject.setBLOBs(id = 37, n = 12, size = 4096, format = "hex")
#'
#' # Same as above, but 0.5MB objects serialized as base64
#' myNewReaRandomObject.setBLOBs(id = 37, n = 12, size = 512, format = "base64")
#'
#' }
#' @family REARandom Constructor Methods
#' @rdname setBLOBs
#' @export setBLOBs
#'

setMethod(f = "setBLOBs",
		  signature("numeric", "numeric", "numeric", "character"),
		  definition = function(id = 42, n = 1, size = 8, format = "base64") {

		  # Check number of random numbers requested
		  if (!(n %in% c(1:100))) stop("Parameter n must be in [1, 100]")

		  # Check the size parameter
		  if ((size %% 8) != 0) stop("Size parameter must be divisible by 8")

		  # Check the format parameter
		  if (!(format %in% c("base64", "hex"))) {
		  	  stop(paste("Format parameter only accepts 'base64' or 'hex'",
		  	  		   "as arguments.  The default is 'base64'."))
		  }

		  # Set the method slot
		  REARandom@method <- "generateBlobs"

		  # Set the ID slot
		  REARandom@id <- id

		  # Set the API method parameters
		  REARandom@parameters <- as.list("apiKey" = .Object@apiKey,
		  								  "n" = n,
		  								  "size" = size,
		  								  "format" = format)

}, valueClass = "REARandom") # End of Method declaration
