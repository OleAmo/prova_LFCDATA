
modosin <- function() {
  lfcMODOSIN$new()
}

lfcMODOSIN <- R6::R6Class(

  classname = "lfcMODOSIN",
  cloneable = FALSE,

  public = list(
    val_1 = NA,
    val_2 = NA,
    res = NA,
    add = function(a,b){
      check_values(a,b)
      self$val_1 <- a
      self$val_2 <- b
    },
    url = function(){
      cat(
        "URL de GitHub = ",
        crayon::blue$underline("https://github.com/OleAmo/prova_LFCDATA)\n")
      )
    },
    sum = function(){
      res <- self$val_1 + self$val_2

      cat(
        "SUMA = " %+% crayon::yellow$bold(paste(self$val_1))%+% " + " %+%
          crayon::yellow$bold(paste(self$val_2)) %+%
          "\n")
      if (res>=0) {
        cat("RESULTADO = " %+% crayon::green$bold(paste(res)))
      } else  {
        cat("RESULTADO = " %+% crayon::red$bold(paste(res)))
      }

    },
    rest = function() {
      res <- self$val_1 - self$val_2
      cat(
       "RESTA = " %+% crayon::yellow$bold(paste(self$val_1))%+% " - " %+%
          crayon::yellow$bold(paste(self$val_2)) %+%
          "\n")
      if (res>=0) {
        cat("RESULTADO = " %+% crayon::green$bold(paste(res)))
      } else  {
        cat("RESULTADO = " %+% crayon::red$bold(paste(res)))
      }

      }
  )
)

check_values = function (a,b){
  if(!is.numeric(a) | !is.numeric(b)){
    stop("valores no numéricos")
  }

}



