#' Function to update the progress of a simple progress bar
#'
#' @param pb Name of the progress bar to update, previously initialised with \code{create_pb()}
#' @param value Level of progress of the bar
#'
#' @return Prints the progress bar with the new value and returns an object of the S4 class \code{simple.progess.bar} containing:
#' \item{message}{A string representing the message to be printed together with the bar}
#' \item{bar}{The progress bar}
#' \item{percentage}{Progress in percentage}
#' \item{length}{Length of the progress bar (integer)}
#' \item{remaining}{Character representing the remaining operations}
#' \item{done}{Character representing the operations already completed}
#' \item{style_msg}{Styling of \code{message} using \code{crayon}}
#' \item{style_bar}{Styling of \code{bar} using \code{crayon}}
#' \item{style_perc}{Styling of \code{percentage} using \code{crayon}}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @import crayon
#' @importFrom utils capture.output
#'
#' @examples
#' # A single bar in a loop
#' pb<-create_pb(length=10, print=TRUE)
#' for(i in 1:10){
#'   cat('This is the number',i,'\n')
#'   pb<-update_pb(pb,i)
#'   Sys.sleep(.3)
#'   cat(pb$value,'operation completed\n')
#' }
#'
#'\donttest{
#' # Two progress bars in multiple loops
#' pb_for<-create_pb(length=3, print=TRUE,colour='red')
#' for(i in 1:3){
#'   cat('This is the number',i,'\n')
#'   pb_for<-update_pb(pb_for,i)
#'   Sys.sleep(1)
#'
#'   pb_while<-create_pb(length=i, print=TRUE,colour='blue')
#'   while(pb_while$value<i){
#'   cat('This is a subtask \n')
#'   pb_while<-update_pb(pb_while,pb_while$value+1)
#'   Sys.sleep(1)
#'   }
#' }
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @export


update_pb<-function(pb,value){
  requireNamespace('crayon')
  loadNamespace('crayon')
  if(!inherits(pb,'simple.progress.bar')){
    stop('pb is not a simple progress bar')
  }
  if(value>pb$length){
    stop('value cannot be larger than the progress bar\'s length')
  }
  pb$bar<-utils::capture.output(cat(
    '[',rep(pb$done,as.integer(value/pb$length*pb$width)),
    rep(pb$remaining,as.integer(pb$width-value/pb$length*pb$width)),']',sep=''
  ))
  pb$message<-ifelse(!is.null(pb$message),
                     paste0('Progress status: ',value,'/',pb$length,'\n'),NULL)
  pb$percentage<-ifelse(!is.null(pb$percentage),
                        paste0(' ',as.integer(value/pb$length*100),'%'),NULL)

  cat(pb$style_msg(pb$message),
      pb$style_bar(pb$bar),
      pb$style_perc(pb$percentage));cat('\n')
  cat('\n')

  pb$value<-value

  return(pb)
}
