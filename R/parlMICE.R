# from https://github.com/gerkovink/parlMICE

#'Parallel function for MICE 
#'
#'This is a wrapper function for \code{\link{mice}}, using multiple cores to 
#'execute \code{\link{mice}} in parallel. As a result, the imputation 
#'procedure is sped up, which might be useful in general.
#'
#'This function is built upon package \code{\link{parallel}}, which is a base
#'package for R versions 2.14.0 and later. We have chosen to use parallel function 
#'\code{parLapply} to allow the use of \code{parallelMICE} on Mac, Linux and Windows
#'systems. For the same reason, we use the Parallel Socket Cluster (PSOCK) type. 
#'
#'On systems other than Windows, it is recommended to change the cluster type to 
#'\code{FORK}, as it is better in handling the memory space. When memory issues 
#'arise on a Windows system, we advise to store the multiply imputed datasets, 
#'clean the memory by using \code{\link{rm}} and \code{\link{gc}} and make another 
#'run using the same settings. For more tips about dealing with memory problems, 
#'we refer to Max Gordon's document *How-to go parallel in R – basics + tips*.
#'
#'This wrapper function combines the output of \code{\link{parLapply}} with
#'function \code{\link{ibind}} in \code{\link{mice}}. A \code{mids} object is returned
#'and can be used for further analyses. 
#'
#'Note that if a seed value is desired, the seed should be entered to this function
#'with argument \code{seed}. Seed values outside the wrapper function (in an 
#'R-script or passed to \code{\link{mice}}) will not result to reproducible results. 
#'We refer to the manual of \code{\link{parallel}} for an explanation on this matter.  
#'
#'A vignette describing the use of `parlMICE` can be found in the \code{\link{mice}} 
#'package or through Github: https://github.com/gerkovink/parallelMICE/blob/master. 
#'
#'@param data A data frame or matrix containing the incomplete data. Similar to 
#'the first argument of \code{\link{mice}}.
#'@param n.core A scalar indicating the number of cores that should be used. Default
#'is the number of logical cores minus 1. 
#'@param n.imp.core A scalar indicating the number of imputations per core. The 
#'total number of imputations will be equal to n.core * n.imp.core. 
#'@param seed A scalar to be used as the seed value. It is recommended to put the 
#'seed value here and not outside this function, as otherwise the parallel processes
#'will be performed with separate, random seeds. 
#'@param ... Named arguments that are passed down to function \code{\link{mice}} or
#'\code{\link{makeCluster}}. 
#' @param m dont know
#'
#'@return A mids object as defined by \code{\link{mids-class}}
#'
#'@author Rianne Schouten, Gerko Vink, 2016, with many thanks to Max Gordon. 
#'@seealso \code{\link{parallel}}, \code{\link{parLapply}}, \code{\link{makeCluster}},
#'\code{\link{mice}}, \code{\link{mids-class}}, Vignette \emph{Wrapper function parlMICE}
#'@references 
#'Gordon, M. (2015). How-to go parallel in R – basics + tips. Available through 
#'[link](http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/)
#'
#'Van Buuren, S. (2012). \emph{Flexible imputation of missing data.} 
#'Boca Raton, FL.: Chapman & Hall/CRC Press.
#'@examples
#'\dontrun{
#'# 150 imputations in dataset nhanes, performed by 3 cores  
#'result1 <- parlMICE(data = nhanes, n.core = 3, n.imp.core = 50)
#'# Making use of arguments in \code{mice}. 
#'result2 <- parlMICE(data = nhanes, method = "norm.nob", m = 100)
#'with(result2, lm(bmi ~ hyp))
#'# On systems other than Windows, use type = "FORK"
#'result3 <- parlMICE(data = nhanes, type = "FORK", n.imp.core = 100)
#'}
#' @importFrom parallel makeCluster clusterExport clusterEvalQ clusterSetRNGStream
#' @importFrom mice mice
#' @export
parlMICE <- function(data, n.core = parallel::detectCores() - 1, n.imp.core = 2,  
                     seed = NULL, m = NULL, ...){
  cl <- parallel::makeCluster(n.core, ...)
  parallel::clusterExport(cl, varlist = "data", envir = environment())
  parallel::clusterEvalQ(cl, library(mice))
  if (!is.null(seed)) {
    parallel::clusterSetRNGStream(cl, seed)
  }
  if (!is.null(m)) {
    n.imp.core <- ceiling(m / n.core)
  }
  imps <- parallel::parLapply(cl = cl, X = 1:n.core, fun = function(i){
    mice::mice(data, print = FALSE, m = n.imp.core, ...)
  })
  parallel::stopCluster(cl)
  imp <- imps[[1]]
  if (length(imps) > 1) {
    for (i in 2:length(imps)) {
      imp <- ibind(imp, imps[[i]])
    }
  }
  imp
}