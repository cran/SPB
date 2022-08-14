#' Class of a simple progress bar
#'
#' @importFrom  methods setOldClass
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name simple.progress.bar
#'
#' @examples
#' pb<-create_pb(length=10, print=FALSE)
#' class(pb)

setOldClass(Classes = 'simple.progress.bar')

#' Print method for simple progress bars
#'
#' @param x A simple progress bar
#'
#' @return Nothing. Prints the progress bar to the console
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @import methods
#'
#' @examples
#'
#' # Example without custom colours or custom message
#' pb<-create_pb(length=10, print=FALSE)
#' print(pb)
#'
#' @export

setMethod(f = 'print',signature(x='simple.progress.bar'),
          definition = function(x){
          cat(x$style_msg(x$message),
              x$style_bar(x$bar),
              x$style_perc(x$percentage));cat('\n')})

print.simple.progress.bar<-function(pb){
  UseMethod(generic = 'print',object = pb)
}

#' Summary method for simple progress bars
#'
#' @param object A simple progress bar
#'
#' @return A summary including:
#' \item{message}{The message printed before the bar (if any)}
#' \item{status}{The advancement status of the bar}
#' \item{max}{The length of the bar}
#' \item{percentage}{The advancement status of the bar in percentage points}
#' The summary is also printed to the console as a kable (if \code{knitr} is installed). Otherwise it prints out as a normal table.
#'
#' @import methods
#' @importFrom knitr kable
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Example without custom message
#' pb<-create_pb(length=10, print=FALSE)
#' summary(pb)
#'
#' # Example with a custom message
#' pb<-create_pb(length=43, print=FALSE,custom.message='Custom pb')
#' summary(pb)
#'
#' # Example with a custom message and an updated value
#' pb<-create_pb(length=11, print=FALSE,custom.message='A new value:')
#' pb<-update_pb(pb,6)
#' summary(pb)
#'
#' @export

# Summary
setMethod(f = 'summary',signature(object='simple.progress.bar'),
          definition = function(object){
            x<-object
            percentage<-unlist(strsplit(x$percentage,split = ''))[-1]
            percentage<-gsub('%','',percentage)
            percentage<-paste0(percentage,collapse = '')
            percentage<-as.integer(percentage)

            res<-data.frame(message=unlist(strsplit(x$message,'\033[4m',fixed = TRUE))[1],
                       status=x$value,
                       max=x$length,
                       percentage = percentage)
            if(requireNamespace('knitr')){
              print(knitr::kable(res,digits = 0,format = 'rst',align = 'c'))
            } else {
              print(res)
            }
            res
          }
)


summary.simple.progress.bar<-function(pb){
  UseMethod(generic = 'summary',object = pb)
}
