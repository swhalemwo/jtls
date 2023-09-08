

#' exports a dataframe to a spreadsheet for inspection
#'
#' sometimes it is helpful or necessary to have a look at data in a spreadsheet format.
#' view_xl views data in excel (or excel clones)
#' @param data dataframe to be displayed
#' @param browser_xl program for opening data.frame. default is libre office's calc (localc),
#' fast alternative is "gnumeric"
#' @return nothing
#' @export
view_xl <- function(data, browser_xl = "localc") {
    if (interactive()) {
        tmp <- tempfile(fileext = ".csv")
        fwrite(data, tmp)
        browseURL(tmp, browser = browser_xl)
        
    }
}


#' some sorting function
#'
#' pick all obs that adhere to priority_vec[pos], but only if not also matched by higher priority
#' to be used with lapply(seq_along(priority_vec))
#' needs source column that corresponds to values in priority_vec
#' @export
sort_by_priority <- function(dfx, priority_vec, pos) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
   
    prty_vlu <- priority_vec[pos]
    vlus_to_disregard <- priority_vec[0:(pos-1)]
    
    grouping_vars <- intersect(c("iso3c", "year"), names(dfx))

    ## print(grouping_vars)
    ## dfx %>% group_by(iso3c, year) %>%
    dfx %>% group_by_at(c(grouping_vars)) %>%
    ## dfx %>% group_by(iso3c) %>%
        ## first exclude all the higher priorities
        mutate(matched_by_higher_prorities = ifelse(len(intersect(source, vlus_to_disregard))==0, F, T)) %>%
        filter(!matched_by_higher_prorities, source == prty_vlu) %>%
        select(all_of(names(dfx)))
}

nicely_fmt_number <- function(vlu, max_digits = 3) {
    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' round number to nice looking number of decimal places
    #' depends on size: each order of magnitude reduces decimal digits by one
    #' max_digits: how many digits there can be at most

    nbr_digits_before_comma <- floor(log10(abs(vlu)))
    
    ## need to use convoluted if-block because switch only allows integer/string matching
    if (nbr_digits_before_comma < 0) {rnd <- max_digits
    } else if (nbr_digits_before_comma == 0) {rnd <- max_digits - 1
    } else if (nbr_digits_before_comma == 1) {rnd <- max_digits - 2
    } else if (nbr_digits_before_comma > 1) {rnd <- max_digits - 3
    }

    ## add some special cases for 0 and 1: don't need decimal places
    ## if (vlu %!in% c(0,1)) {
    if (vlu %% 1 == 0) { # integer check: don't need decimal points
        format(vlu, nsmall = 0, big.mark = " ")
    } else {
        format(round(vlu, rnd), big.mark = " ", nsmall = max(0,rnd)) # nsmall can't be lower than 0
        
    }
}

#' first try at more automatic decimal points formatting
#'
#' @export
nicely_fmt_number_v <- Vectorize(nicely_fmt_number)

fmt_nbr_flex_single <- function(vlu, digits = 3) {
    #' flexibly format number
    #' if integer: no decimal points
    #' if not integer, round to digits
    #' big numbers always split by space

    
    if (vlu %% 1 == 0) { # integer check: don't need decimal points
        format(vlu, nsmall = 0, big.mark = " ")
    } else {
        format(round(vlu, digits), big.mark = " ", nsmall = digits) # nsmall can't be lower than 0
        
    }
}


#' formats number 
#'
#' might not be so relevant: maybe ok descriptive tables,
#' but in-text numbers seem to require hand-determined number of decimal points
#' @export
fmt_nbr_flex <- Vectorize(fmt_nbr_flex_single)

#' check if function expr leaks memory
#'
#' @export
mem_tester <- function(expr, n) {

    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gc()
    
    for(i in 1:n) {

        mem_start <- as.integer(lobstr::mem_used())

        ## cmd <- substitute(expr)
        ## eval(as.call(cmd))
        eval(substitute(expr))

        ## rm(x)
        gc()
        mem_end <- as.integer(lobstr::mem_used())

        print(mem_end - mem_start)
    }

}
