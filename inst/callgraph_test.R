



f1 <- function(a) {
    a + 10
}

f2 <- function(b) {
    b -5
}

wrap_dragon_magic <- function(func, ...) {

    print('xiao bu')
    result <- func(...)

    attr(result, "gnrtdby") <- as.character(func)
    return(result)
}




time_wrapper <- function(func, ...) {
  # Print the current time
  cat("Current time:", Sys.time(), "\n")
  
  # Call the original function with the provided arguments
  result <- func(...)
  
  return(result)
}


f1(5)

result_f1 <- time_wrapper(f1, 5)  # Call f1 with argument 5


track_function <- function(fn) {
    ## Define an inner function that calls the original function and adds an attribute
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    
    funcname <- deparse(substitute(fn))
    print(funcname)

    inner <- function(...) {
        print(match.call())
        ## print(call())
        result <- fn(...)
        
        ## attr(result, "generated_by") <- deparse(substitute(fn))
        ## attr(result, "generated_by") <- deparse(quote(fn))
        attr(result, "generated_by") <- funcname
        return(result)
    }
    
    return(inner)
}

f1 <- function(a) {
    11
    22
    33
    
    a + 10
}

f2 <- function(a) {
    print(match.call())
    a + 10}


fx1 <- track_function(f1)

vx <- 10
xx <- fx1(vx)
yy <- f2(vx)


## difference is this: one has (vx), other has (a=vx)
## not sure if the "a =" part is needed.. atm not caring about which argument goes where..
## > xx <- fx1(vx)
## fx1(vx)
## > yy <- f2(vx)
## f2(a = vx)

attr(xx, "generated_by")

sqrt2 <- track_function(sqrt)
yy <- sqrt2(9)


f3 <- function() {
    data.table(mtcars)
}

f3x <- track_function(f3)
f3x()

## issues:
## can't see the function code of untracked function: could print make some hacks tho (add _ut) to it etc
## can't easily debug: would also have to use hack to use untracked version


## try trace

f4 <- function(x) {
    y <- x+5
    z <- y+y %>% sqrt %>% adt

    return(z)
}

helper <-function(){
    eval(quote(print(x)), parent.frame())
}

helper2 <-function(){
    eval(quote(print(match.call())), parent.frame())

    
    eval(quote(z <- 3), parent.frame())
}


printer <- function() print('hi')
printer_x <- function() print(x)

trace(f4, tracer = helper, at =1)
trace(f4, helper2, at= 4)
untrace(f4)

library(methods)

f4(3)
bf4 <- body(f4)

strx <- "function(x) {x + 5
    y <- x+5
    z <- y+y %>% sqrt %>% adt
    return(z)
}"

f4 <- function(x) {
    y <- x+5
    z <- y+y %>% sqrt %>% adt

    return(z)
}




## shift the instructions by one, duplicating the first real one (second technical one, first technical is {
for (i in len(body(f4)):2) {
    body(f4)[[i+1]] <- body(f4)[[i]]
}



body(f4)[[2]] <- substitute(gw_fargs(match.call()))
f4(10)



## strangely only for-loop works, map/walk doesn't
## walk(len(body(f4)):1, ~(body(f4)[[.x+1]] <- body(f4)[[.x]]))
## walk(len(body(f4)):2, ~assign(body(f4)[[.x+1]], body(f4)[[.x]]))

getSrcFilename(f4)



map(len(body(f4)):1)

f4b <- str2lang(strx)
assign("f4b", eval(str2lang(strx)))


f4b(3)

lx <- as.list(body(f4))

lex <- as.expression(lx)

body(f4)[[2]][[3]][[3]] <- substitute(2+5)
f4




f5 <- function(dd = 5) {
    dd + 10
}




expression({x+2})

(ess-command ".ess_dbg_flag_for_debuging('f4')\n")

.ess_dbg_flag_for_debuging('f4')
.ess_dbg

(ess-command "print(4)")


my_function <- function(x) {
    result <- x^2
    return(result)
}

