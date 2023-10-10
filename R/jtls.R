
#' @import magrittr
#' @import Rgraphviz
#' @import graph
#' @import data.table
#' @importFrom mvbutils foodweb

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
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## get config
    c_plt <- chuck(c_plts, pltname)
    
    ## write to file
    pdf(paste0(chuck(c_dirs, "figs"), pltname, ".pdf"),
        width = chuck(c_plt, "width")/2.54, height = chuck(c_plt, "height")/2.54)
    plot(chuck(l_plts, pltname))
    dev.off()

    png(paste0(chuck(c_dirs, "figs"), pltname, ".png"),
        width = chuck(c_plt, "width")/2.54, height = chuck(c_plt, "height")/2.54, units = "in", res=300)
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

    filename <- paste0(chuck(c_dirs, "figs"), pltname, ".pdf")

    ## checking whether file is open:
    ## copied from custom_funcs, used in reg_anls.R
    ## gonna be fun porting that over..

    open_check_cmd <- paste0("lsof -w ", filename)
    open_check_res <- system(open_check_cmd, intern = T)

    open <- is.null(attr(open_check_res, "status"))
    
    if (!open)  {
        open_cmd <- paste0("zathura ", filename, " &")
        system(open_cmd)
    } else if (open) {
        print(sprintf("%s already open", pltname))
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
#' @return nothing (invisible(TRUE))
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


#' generate the ynkplt configs which direct plot insertion via org-macro
#'
#' uses calls project-specific gc_plts (generation of config of plots) function, 
#' thus gc_plts has to be defined, and return list of lists, which keys as plot names,
#' and key values as lists containing caption, width, height
#' 
#' @export        
gc_ynkplt <- function() {
    do.call("gc_plts", list()) %>% 
        ## gc_plts() %>%
        imap(~.x[.c(width, caption)]) %>% rbindlist(idcol = "pltname") %>% # height not needed
        .[, macro :=
                sprintf(paste0('(eval (concat "#+label: fig:%s\\n" "#+caption: %s\\n" ',
                               '"#+attr_latex: :width %scm\\n" "[[file:../figures/%s]]"))'),
                        pltname, 
                        caption,
                        width,
                        ## paste0("plt_", batch_version, "_", filename))] %>%
                        paste0(pltname, ".pdf"))] %>%
        .[, .(nbr_name = paste0("ynkplt_", pltname), nbr_fmt = macro, grp = "pltcfgs")]
}

#' generate the (in-text) references to plots
#'
#' @export
gc_refplt <- function() {

    do.call("gc_plts", list()) %>%
        imap(~.x[.c(width, caption)]) %>% rbindlist(idcol = "pltname") %>%
        .[, .(nbr_fmt = sprintf("\\ref{fig:%s}", pltname),
              nbr_name=  paste0("refplt_", pltname),
              grp = "figlbls")]
}
    
    


#' generate the macros in the actual org-file
#'
#' @export
wd_nbrs <- function(dt_nbrs) {
    dt_nbrs[, .(macro_str = sprintf("#+MACRO: %s %s", nbr_name, nbr_fmt)), grp] %>%
        .[, .(grp_str = paste(macro_str, collapse = "\n")), grp] %>%
        .[, .(grp_str2 = paste0("# ", grp, "\n", grp_str))] %>% 
        .[, paste0(grp_str2, collapse = "\n\n\n")]
}



#' generate and write to file the arguments a function is called with to generate callgraph
#' 
#' "gw_fargs(match.call())" has to be inserted in a function definition
#' uses the original object name used in parameter via NES
#' works with do.call() as long as stuff is quoted (what it should be for data objects of substantial size)
#' requires c_dirs: list of directories, one of them named "tbls": appends to a csv file there,
#' which is later read by "cl_clgr_objs"
#' @export
#' @param matched_call: just put match.call() there
gw_fargs <- function(matched_call) {
    callstr <- matched_call %>% deparse

    fname <- as.character(matched_call[[1]])

    ## nchar(call_str)
    fargs_str <- substring(callstr, nchar(fname) + 2, nchar(callstr)-1) # remove function name and brackets

    ## parse function arguments
    dt_fargs <- strsplit(fargs_str, ",")[[1]] %>% 
        lapply(\(x) strsplit(x, "=")[[1]] %>% trimws %>%
                setNames(c("param_name", "param_value")) %>% as.list) %>% rbindlist

    ## reorder columns
    dt_fargs <- dt_fargs[, .(fname = fname, param_name, param_value)]

    fwrite(dt_fargs, paste0(c_dirs$tbls, "farg_calls.csv"), append = T)
}



#' generate function links with mvbutils::foodweb
#'
#' @export
#' @return list with dataframes for edges and nodes
gl_funlinks <- function() {
  
    fw <- mvbutils::foodweb(plotting = F, where = 1) # funs = ls())


    funcs_to_yeet <- .c(atb, adt,len, print.data.table, print_data_table, adf, achr, anum, gw_fargs, gl_funlinks,
                        gl_funmat, gg_clgrph, gc_locs, gc_plts, gd_nbrs, gl_clgr_objs, gc_clgrphattrs)

    dt_funmat_edges_prep <- fw$funmat[rownames(fw$funmat) %!in% funcs_to_yeet, # yeet unneeded rows
                                      colnames(fw$funmat) %!in% funcs_to_yeet] %>%  # yeet unneeded columns
        adt(keep.rownames = "caller") %>% # convert to dt
        melt(id.vars = "caller", variable.name = "called") %>% # melt into long
        .[, called := as.character(called)] # somehow factor 
    
    ## create edge dt
    list(
        edges = dt_funmat_edges_prep[value == 1, .(caller, called)],
        nodes = data.table(fun = unique(dt_funmat_edges_prep$caller))
        )
    
    ## node dt: based on square matrix: every function is in both columns and rows -> using orig rows is enough
}

#' combine funlinks with additional custom links:
#'
#' gnrtdby: objected attribute to see which object is generated by which function
#' fargs: function arguments: see which argument is used by a function
#' both require having run the entire codebase
#' @export
#' @return list of nodes and edges
gl_clgr_objs <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
   
    ## process the gnrtdby attributes
    l_objs_gnrtdby <- keep(ls(envir = globalenv()), ~("gnrtdby" %in% names(attributes(get(.x)))))
    
    if (len(l_objs_gnrtdby) > 0) {
        dt_gnrtdby <- l_objs_gnrtdby %>% 
        map(~list(obj_name = .x, gnrtdby = attr(get(.x), "gnrtdby"))) %>% rbindlist %>%
        .[, .(caller = obj_name, called = gnrtdby)]
    } else if (len(l_objs_gnrtdby) == 0) {
        dt_gnrtdby <- data.table(caller = character(), called = character())
    }
        

    ## evaluate the farg_calls file (input arguments)
    dt_fargs <- fread(paste0(c_dirs$tbls, "farg_calls.csv"))[, .(caller = fname, called = param_value)] %>%
        unique %>% 
        .[is.na(as.logical(called))] # filter out logical switches

    ## combine into object-funtion links
    dt_obfn_links <- rbind(dt_gnrtdby, dt_fargs)

    l_funlinks <- gl_funlinks()

    list(
        edges = Reduce(rbind, list(dt_gnrtdby, dt_fargs, l_funlinks$edges)),
        nodes = data.table(node = unique(c(l_funlinks$nodes$fun, dt_obfn_links$called, dt_obfn_links$caller)))
    )
}


#' generate a graph (Rgraphviz package)
#'
#' @param l_gobjs list of dataframes: nodes, edges
#' @export
#' @return graph object
gg_clgrph <- function(l_gobjs) {
    #' generate a new graph
    #' l_gobjs: list of graph objects:
    #' - nodes: dt with column node
    #' - edges: dt with columns of caller/called

    gx <- new("graphNEL", nodes = l_gobjs$nodes$node, edgemode = "directed")
    ## just write shitty for-loop
    for (i in seq(1,fnrow(l_gobjs$edges))) {
        ## print(i)
        gx <- addEdge(l_gobjs$edges[i, called], l_gobjs$edges[i, caller], gx)
    }

    return(gx)
}



#' generate, write and display the callgraph
#'
#' uses c_dirs and dpltF
#' @export
gwd_clgrph <- function() {

    px <- gl_clgr_objs() %>% gg_clgrph

    pdf(paste0(c_dirs$figs, "callgraph.pdf"), height = 3, width = 6)
    renderGraph(layoutGraph(px, attrs = gc_clgrphattrs()))
    dev.off()

    dpltF("callgraph")
}


#' quick way of getting model object names written as model names
#'
#' uses NSE -> doesn't work with list of models previously defined
#' also requires a list (can't just use a single model as in screenreg)
#'
#' basically intended for quick and dirty model results inspection where I'm not sure
#' if i'll write a proper model exporting/formating function
#' @export
#' @param mdl_list list of regression models
#' @param ... other screenreg arguments
screenreg2 <- function(mdl_list, ...) {

    input_string <- match.call() %>% deparse
    pattern <- "mdl_list = list\\(.*?\\)"

    ## Extract the model list argument 
    mdl_names_prep1 <- regmatches(input_string, gregexpr(pattern, input_string)) %>% chuck(1)

    ## yeet the "mdl_list = list"
    c_mdl_names <- substring(mdl_names_prep1, 17, nchar(mdl_names_prep1)-1) %>%
        strsplit(",") %>% unlist %>% trimws

    ## pass custom model names as argument, all other arguments stay
    screenreg(mdl_list, custom.model.names = c_mdl_names, ...)


}
