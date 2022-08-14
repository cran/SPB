#' Function to create a simple progress bar
#'
#' @param length Length of the progress bar. For instance, in a loop it would be the number of iterations to complete.
#' @param remaining The character (string of unitary length) used to represent operations still to do.
#' @param done The character (string of unitary length) used to represent operations already completed
#' @param percentage Logical |  Whether to print the percentage progess after the bar. Defaults to \code{TRUE}
#' @param message Logical |  Whether to print a message before the progress bar. Defaults to \code{TRUE}
#' @param custom.message Optional message to be printed before the progress bar. Defaults to \code{NULL}, which prints simply \code{'Progress status:'},
#' @param trim How many points to trim from the maximum length of the terminal output. Default to \code{6L}
#' @param print Logical | Whether to print the progress bar after its initialisation. Defaults to \code{TRUE}
#' @param colour Styling of the progress bar object. It can be a single string, in which case it applies to all the part of the progress bar (message, bar and percentage). It can also be a vector of length 3, in which case each string applies to one of the items (respectively message, bar and percentage). The strings must be taken amongst the following values:
#'   - black;
#'   - blue;
#'   - cyan;
#'   - green;
#'   - magenta;
#'   - red;
#'   - white;
#'   - yellow.
#' @param bg.colour Styling of the progress bar object's background. It can be a single string, in which case it applies to all the part of the progress bar (message, bar and percentage). It can also be a vector of length 3, in which case each string applies to one of the items (respectively message, bar and percentage). The strings must be taken amongst the following values:
#'   - Black;
#'   - Blue;
#'   - Cyan;
#'   - Green;
#'   - Magenta;
#'   - Red;
#'   - White;
#'   - Grey;
#'   - Yellow.
#'
#' @return An object of the S4 class \code{simple.progess.bar} containing:
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
#'
#' # Example without custom colours or custom message
#' pb<-create_pb(length=10, print=TRUE)
#'
#' # Example without custom colours but with custom message
#' pb<-create_pb(length=10, custom.message = 'Custom progress message:',
#' print = TRUE)
#'
#' # Example with custom colours and custom message
#' pb<-create_pb(length=10, custom.message = 'Custom progress message:',
#' print = TRUE,colour = c('magenta','red','blue'))
#'
#' @export


create_pb<-function(length,remaining='-',done='=',percentage=TRUE,message=TRUE,
                    custom.message=NULL,trim=6L,print=TRUE,
                    colour='red', # one colour or three
                    bg.colour='white' # one colour or three
){
  requireNamespace('crayon')
  loadNamespace('crayon')

  # Check length as integer
  if(!is.integer(length)){
    length<-as.integer(length)
    if(is.na(length)){
      stop('Length is not a number!')
    }else{
      warningCondition('Coercing length to an integer')
    }
  }
  # Check remaining as single character
  if(nchar(remaining)>1){
    remaining<-unlist(strsplit(as.character(remaining),'',fixed = TRUE))[1]
    warningCondition('The parameter \'remaining\' was longer than a character.\n
                     Considering only the first character')
  }
  # Check done as single character
  if(nchar(done)>1){
    remaining<-unlist(strsplit(as.character(done),'',fixed = TRUE))[1]
    warningCondition('The parameter \'done\' was longer than a character.\n
                     Considering only the first character')
  }
  # Check booleans
  if(!all(is.logical(percentage)&is.logical(message)&is.logical(print))){
    if(!is.logical(percentage)){
      percentage<-TRUE
      warningCondition('The parameter \'percentage\' should be logical!\n
                       restoring default')
    }
    if(!is.logical(message)){
      message<-TRUE
      warningCondition('The parameter \'message\' should be logical!\n
                       restoring default')
    }
    if(!is.logical(print)){
      print<-TRUE
      warningCondition('The parameter \'print\' should be logical!\n
                       restoring default')
    }
  }

  # Width
  width<-as.integer(options()$width-trim)

  # Message
  if(message==FALSE&&!is.null(custom.message)){
    warningCondition('A custom message was provided, but messages were
                     deactivated. No message will be printed')
  } else {
    custom.message<-ifelse(is.null(custom.message),
                            'Progress status:',
                            custom.message)
    msg<-ifelse(message,
                paste0(custom.message,' ',crayon::underline(0,'/',length),'\n'),NULL)
  }

  # Bar
  bar<-utils::capture.output(cat('[',rep(done,as.integer(0/length*width)),
                          rep(remaining,as.integer(width-0/length*width)),']',
                          sep=''))

  # Percentage
  perc<-ifelse(percentage,paste0(' ',as.integer(0/length),'%'),NULL)

  # Text colour
  colour<-tolower(colour)
  if(length(colour)==3){
    col_msg<-colour[1]
    col_bar<-colour[2]
    col_perc<-colour[3]
  } else {
    col_msg<-col_bar<-col_perc<-colour
  }

  # Text background
  if(bg.colour!='White'&bg.colour!='white'){
    if(bg.colour=='Grey'|bg.colour=='Gray')bg.colour<-'White'
    bg.colour<-paste0(toupper(unlist(strsplit(bg.colour,''))[1]),
                      paste(unlist(strsplit(bg.colour,''))[-1],collapse = ''))

    bg.colour<-paste0('bg',bg.colour)
    if(length(bg.colour)==3){
      bg_msg<-bg.colour[1]
      bg_bar<-bg.colour[2]
      bg_perc<-bg.colour[3]
    } else {
      bg_msg<-bg_bar<-bg_perc<-bg.colour
    }
  } else {
    bg_msg<-bg_bar<-bg_perc<-'black'
  }


  # Text styles
  style_msg<-crayon::combine_styles(bg_msg,col_msg)
  style_bar<-crayon::combine_styles(bg_bar,col_bar)
  style_perc<-crayon::combine_styles(bg_perc,col_perc)

  if(print)cat(style_msg(msg),
               style_bar(bar),
               style_perc(perc));cat('\n')

  pb<-list(message=msg,bar=bar,percentage=perc,width=width,
          length=length,remaining=remaining,done=done,value=0L,
          style_msg=style_msg,style_bar=style_bar,style_perc=style_perc)
  class(pb)<-'simple.progress.bar'
  pb
}
