
#' @import magrittr

.datatable.aware = T


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

#' writes plots
#'
#' to be used for writing plot
#' 
#' @param: pltname
#' @export
wplt <- function(pltname, c_plts = do.call("gc_plts", c_pltargs)) {

    ## get config
    c_plt <- chuck(c_plts, pltname)
    
    ## write to file
    pdf(paste0(FIG_DIR, pltname, ".pdf"), width = chuck(c_plt, "width"), height = chuck(c_plt, "height"))
    plot(chuck(l_plts, pltname))
    dev.off()
    
}

#' display pltname in R
#'
#' to be used when plot is supposed to be displayed/inspected
#' fetches pltname from global l_plts
#' @param pltname plt to
#' @return ggplot object
#' @export
dpltR <- function(pltname) {
    return(chuck(l_plts, pltname))
}

#' display pltname from file
#'
#' more helper function, but still worth to make it global
#' @param pltname: plt to display
#' @export
dpltF <- function(pltname) {

    filename <- paste0(FIG_DIR, pltname, ".pdf")

    ## checking whether file is open:
    ## copied from custom_funcs, used in reg_anls.R
    ## gonna be fun porting that over..

    open_check_cmd <- paste0("lsof -w ", filename)
    open_check_res <- system(open_check_cmd, intern = T)

    open <- is.null(attr(open_check_res, "status"))

    if (!open)  {
        open_cmd <- paste0("zathura ", filename, " &")
        system(open_cmd)
    }
         
}


#' generate and display plot (intended to use after function change)
#'
#' to be used when plot is supposed to be displayed/inspected
#' @param pltname plot to generate and display 
#' @export
gdplt <- function(pltname) {

    ## generate plot, assign it to global l_plts
    gplt(pltname)
    ## fetch it from l_plts
    return(dpltR(pltname))
}

#' generate, write and display plot as pdf
#'
#' @param pltname plot to generate, write and display
#' @export
gwdplt <- function(pltname) {
    gplt(pltname)
    wplt(pltname)
    dpltF(pltname)
    
}

#' write and display plot as pdf
#' 
#' intended for resolution checks
#' @param pltname plot to write and display
#' @export
wdplt <- function(pltname) {

    wplt(pltname)
    dpltF(pltname)
}


#' generate plot corresponding to pltname
#'
#' assigns it via side-effect to l_plts.
#' @param pltname string of plot to plot. requires that paste0("g_", pltname) is a function that returns a ggplot
#' @param c_plts plot configuration, by default generated by project-specific gc_plts function:
#' returns list of lists of plot parameters
#' @export
gplt <- function(pltname, c_plts = do.call("gc_plts", c_pltargs)) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## select pltname cfg'; strip caption, width, caption 
    c_plt <- chuck(c_plts, pltname) %>% .[names(.) %!in% c("caption", "width", "height")]
    

    ## generate the plot
    p_x <- do.call(paste0("g", pltname), c_plt)

    ## assign to l_plts
    ## `pluck<-`(l_plts, pltname, value = p_x)
    l_plts[[pltname]] <<- p_x

    return(invisible(TRUE))
               
}



        
