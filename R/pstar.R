#' Internal function to create significance stars for a p value.
#' 
#' @param p a p value
pstar = function(p){
  if(p<.001){
    s = "***"
  }else if(p<.01){
    s = "**"
  } else if(p<.05){
    s = "*"
  } else if(p<.01){
    s = "^"
  } else{
    s = ""
  }
  return(s)
}