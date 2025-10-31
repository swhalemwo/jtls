
#' @import magrittr
#' @import data.table
#' @importFrom broom tidy glance
#' @importFrom mvbutils foodweb
#' @importFrom xtable xtable print.xtable
#' @importFrom texreg coeftostring
#' @importFrom Hmisc latexTranslate
#' @importFrom DBI dbDataType dbSendQuery dbRemoveTable dbAppendTable dbConnect dbGetQuery
#' @importFrom collapse char_vars
#' @importFrom kit pall
#' @importFrom nodbi src_sqlite docdb_create docdb_get
#' @importFrom tidygeocoder geocode
#' @importFrom RSQLite SQLite
#' @importFrom xgboost xgboost xgb.train xgb.DMatrix getinfo xgb.importance
#' @importFrom stringdist stringdist
.datatable.aware = T

## ' @import graph


#' returns T if for each element of x that is included in y
#'
#' @param x a vector 
#' @param y a vector
#' @export
'%!in%' <- function(x,y)!('%in%'(x,y))

#' generate list of directory for a project
#'
#' @param dir_proj the project directory, use full path just to be sure
#' @export
gc_dirs <- function(dir_proj) {
    list(
        proj = dir_proj,
        figs = paste0(dir_proj, "figures/"),
        tbls = paste0(dir_proj, "tables/"),
        data = paste0(dir_proj, "data/"),
        code = paste0(dir_proj, "scripts/"),
        misc = paste0(dir_proj, "misc/")
    )
}


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
#' pick all obs that adhere to priority_vec \[ pos \], but only if not also matched by higher priority
#' to be used with lapply(seq_along(priority_vec))
#' needs source column that corresponds to values in priority_vec
#' @param dfx data.frame to sort
#' @param priority_vec vector of priorities
#' @param pos position
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
        mutate(matched_by_higher_prorities = ifelse(length(intersect(source, vlus_to_disregard))==0, F, T)) %>%
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

    open <- check_if_file_is_open(filename)

    ## open_check_cmd <- paste0("lsof -w ", filename)
    ## open_check_res <- system(open_check_cmd, intern = T)

    ## open <- is.null(attr(open_check_res, "status"))
    
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
        imap(~.x[c("width", "caption")]) %>% rbindlist(idcol = "pltname") %>% # height not needed
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
        imap(~.x[c("width", "caption")]) %>% rbindlist(idcol = "pltname") %>%
        .[, .(nbr_name=  paste0("refplt_", pltname),
              nbr_fmt = sprintf("\\ref{fig:%s}", pltname),
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
    callstr <- matched_call %>% deparse(width.cutoff = 500)

    fname <- as.character(matched_call[[1]])

    ## nchar(call_str)
    fargs_str <- substring(callstr, nchar(fname) + 2, nchar(callstr)-1) # remove function name and brackets

    ## parse function arguments
    ## arguments are split on first equal sign, so args with an equal sign (e.g. dt[dd == 2]) preserved
    dt_fargs_prep <- strsplit(fargs_str, ",")[[1]] %>% 
        lapply(\(x) strsplit(sub("=", "splitmehere", x), "splitmehere")[[1]] %>% trimws %>%
                    setNames(c("param_name", "param_value")) %>% as.list) %>% rbindlist
    
    ## clean up all kinds of weird param_values
    
    ## reorder columns
    dt_fargs <- dt_fargs_prep[, .(fname = fname, param_name, param_value = gsub("\\[.*?\\]", "", param_value))] %>%
        .[, param_value := gsub('"', "", param_value)]

    fwrite(dt_fargs, paste0(c_dirs$misc, "farg_calls.csv"), append = T)
}



#' generate function links with mvbutils::foodweb
#'
#' @export
#' @return list with dataframes for edges and nodes
gl_funlinks <- function() {
  
    fw <- mvbutils::foodweb(plotting = F, where = 1) # funs = ls())


    funcs_to_yeet <- c("atb", "adt", "len", "length", "print.data.table", "print_data_table", "adf", "achr", "anum",
                       "gw_fargs", "gl_funlinks", "gl_funmat", "gg_clgrph", "gc_dirs", "gc_plts", "gd_nbrs",
                       "gl_clgr_objs", "gc_clgrphattrs", "gc_tbls", "gc_vvs")

    dt_funmat_edges_prep <- fw$funmat[rownames(fw$funmat) %!in% funcs_to_yeet, # yeet unneeded rows
                                      colnames(fw$funmat) %!in% funcs_to_yeet] %>%  # yeet unneeded columns
        adt(keep.rownames = "caller") %>% # convert to dt
        melt(id.vars = "caller", variable.name = "called") %>% # melt into long
        .[, called := as.character(called)] # somehow factor 
        

    ## create edge dt
    list(
        edges = dt_funmat_edges_prep[value == 1, .(caller, called, linktype = "fun-fun")],
        nodes = data.table(fun = unique(dt_funmat_edges_prep$caller))
        )
    
    ## node dt: based on square matrix: every function is in both columns and rows -> using orig rows is enough
}

rec_attr_query <- function(item_name, parent = "global", prevlinks = list()) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    
    ## doesn't seem like get() supports nested children %>% (ab)use eval(parse())
    subitem_names_all <- eval(parse(text = sprintf("names(%s)", item_name)))
    
    if (length(subitem_names_all) > 0) {
    
        ## reconstruct full item paths
        subitem_fullpaths <- paste0(fifelse("$" %in% item_name, parent, item_name), "$", subitem_names_all)
    } else {
        subitem_fullpaths <- c()
    }
    
    ## keep those that have a gnrtdby attribute: first construct filter
    rel_fltr <- map_vec(subitem_fullpaths, ~!is.null(attr(eval(parse(text = .x)), "gnrtdby")))
    
    ## subitem_names_rel <- keep(subitem_fullpaths, ~!is.null(attr(eval(parse(text = .x)), "gnrtdby")))

    ## then apply filter
    subitem_fullpaths_rel <- subitem_fullpaths[rel_fltr]
    subitem_names_rel <- subitem_names_all[rel_fltr]
    

    ## flatlist <- map(subitem_names_rel, ~list(item = .x, parent = item_name))
    flatlist <- map2(subitem_fullpaths_rel, subitem_names_rel,
                     ~list(item = .x, parent = item_name, display_name = .y))

    ## prevlinks <- list()
    ## prevlinks <- c(list(prevlinks), map(subitem_names_rel, ~list(item = .x, parent = parent)))a

    ## initiation
    if (length(prevlinks) == 0) {
        prevlinks <- flatlist

    } else {
        ## only append items when there are items to append
        if (length(flatlist) > 0) {
            prevlinks <- c(prevlinks, flatlist)
        }
    }
            
    if (length(subitem_names_rel) > 0) {
        ## if there is at least one sub-item: go one level deeper

        ## print("adding")
        ## print(sprintf("before lapply: prevlinks = %s", paste(prevlinks, collapse = " ")))
                
        ## result of lapply has to be flattened for some reason, otherwise gets nested more and more 
        lapply(subitem_fullpaths_rel, \(x) rec_attr_query(x, parent = item_name, prevlinks = prevlinks)) %>% flatten
        
        
        ## print(sprintf("after lapply: prevlinks = %s", prevlinks))
        
    
    } else {
        ## print("not adding")
        ## print(sprintf("before returning: prevlinks = %s", paste(prevlinks, collapse = " ")))
        ## return the object and its parent
        ## this should be the called when in the last leaf where there are no relevant subitems
        ## only itself to return, as child of its parent
    
        ## return(c(prevlinks, list(item = item_name, parent = parent)))
        ## print(sprintf("item_name %s, prevlinks: %s", item_name, prevlinks))
        
        return(prevlinks)

    }
 
    
   
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
    
    ## recursively iterate over all the l_objs_gnrtdby to see which subitems have also been gnrtd

    if (length(l_objs_gnrtdby) > 0) {

        ## get the function that global objects are have been generated by by querying their classes
        dt_gnrtdby <- l_objs_gnrtdby %>% 
            map(~list(obj_name = .x, gnrtdby = attr(get(.x), "gnrtdby"))) %>% rbindlist %>%
            .[, .(caller = obj_name, called = gnrtdby)] %>%
            .[, `:=`(linktype = "obj-fun")]

        ## get the relationships of parents to children to assign children/subitems to their parents
        ## for children that have gnrtdby attribute (as long as their parents have it too), 
        
        ## these are not links, these are the subgraphs -> don't bind them with the other links
        dt_parchild_rels_prep <- map(l_objs_gnrtdby, ~rbindlist(rec_attr_query(.x))) %>% list_rbind

        ## also generate some empty dt_parchild_rels if there are none
        ## FIXME: this is refered later on
        if (nrow(dt_parchild_rels_prep) > 0) {
            dt_parchild_rels <- dt_parchild_rels_prep %>% unique %>% .[, linktype := "par-child"]
        } else {
            dt_parchild_rels <- data.table(item = character(), parent = character(),
                                           linktype = character(), display_name = character())
        }
        
        ## rec_attr_query(l_objs_gnrtdby[[1]])

    } else if (length(l_objs_gnrtdby) == 0) {
        dt_gnrtdby <- data.table(caller = character(), called = character(), linktype = character())
                                 ## display_name = character())
    }
        

    ## evaluate the farg_calls file (input arguments)

    f_farg_calls <- paste0(c_dirs$misc, "farg_calls.csv")

    if (file.exists(f_farg_calls)) {
        dt_fargs <- fread(f_farg_calls)[, .(caller = fname, called = param_value)] %>%
            unique %>% 
            .[is.na(as.logical(called))] %>% # filter out logical switches
            .[, linktype := "fun-obj"]
    } else {
        dt_fargs <- data.table(caller = character(), called = character(), linktype = character())
    }
        

    ## combine into object-funtion links
    dt_obfn_links <- rbind(dt_gnrtdby, dt_fargs)

    l_funlinks <- gl_funlinks()

    ## best way to store clusters: in the nodes dt?
    ## can also update display name there
    dt_nodes <- data.table(node = unique(c(l_funlinks$nodes$fun, dt_obfn_links$called,
                                           dt_obfn_links$caller, dt_parchild_rels$item))) %>% 
        dt_parchild_rels[, .(item, parent, display_name)][., on = .(item = node)]

    ## need to create the fake cluster nodes: to get 
    dt_clusters <- dt_nodes[!is.na(parent), .(unq_parent = unique(parent))] %>%
        .[, cluster := paste0("cluster_", unq_parent)]

    ## add cluster info to the cluster nodes
    dt_nodes_clustered <- dt_clusters[dt_nodes, on = .(unq_parent = item)]

    ## add cluster into to nodes inside the cluster with update join
    ## i hate the anti-christ
    dt_nodes_clustered[dt_clusters, on = .(parent = unq_parent), cluster := i.cluster]
    dt_nodes_clustered[, parent := NULL] # parent column no longer needed
    

    dt_edges <- Reduce(rbind, list(dt_gnrtdby, dt_fargs, l_funlinks$edges))

    ## adjust edges to end/begin in clusters: merge once witih caller, once with called
    ## the original cluster node will shrink to nothing, but still be there ->
    ## links to/from clusters have to respect cluster boundaries
    dt_edges_clustered <- merge(dt_edges, dt_clusters, by.x = "caller", by.y = "unq_parent", all.x= T) %>%
        .[, edge_end := fifelse(!is.na(cluster), cluster, caller)] %>%
        .[, cluster := NULL] %>% # delete cluster column after caller query 
        merge(., dt_clusters, by.x = "called", by.y = "unq_parent", all.x = T) %>% 
        .[, edge_start := fifelse(!is.na(cluster), cluster, called)] %>% .[, cluster := NULL]

    l_clgrph_objs <- list(
        edges = dt_edges_clustered,
        nodes = dt_nodes_clustered
    )

    return(l_clgrph_objs)

}


## #' generate a graph (Rgraphviz package)
## #'
## #' @param l_gobjs list of dataframes: nodes, edges
## #' @export
## #' @return graph object
## gg_clgrph <- function(l_gobjs) {
##     #' generate a new graph
##     #' l_gobjs: list of graph objects:
##     #' - nodes: dt with column node
##     #' - edges: dt with columns of caller/called

##     gx <- new("graphNEL", nodes = l_gobjs$nodes$node, edgemode = "directed")
##     ## just write shitty for-loop
##     for (i in seq(1,fnrow(l_gobjs$edges))) {
##         ## print(i)
##         gx <- addEdge(l_gobjs$edges[i, called], l_gobjs$edges[i, caller], gx)
##     }

##     return(gx)
## }


#' custom parsing of l_gobjs (list of dts with nodes and edges) to dot language
#' somewhat ugly but necessary to get subgraphs (not supported by Rgraphviz or DiagrammeR)
#'
#' @param l_gobjs list of data.tables, nodes and edges
#' @return parsed (string) representation of callgraph 
parse_clgrph_todot <- function(l_gobjs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## parse the nodes first: add display names where they exist
    dt_nodes_labeled <- copy(l_gobjs$nodes) %>%
        .[, unq_parent := gsub("\\$", "__", unq_parent)] %>% # replace dollar signs (not liked by graphviz)
        .[, node_parsed := paste0(unq_parent, 
                                  fifelse(!is.na(display_name), sprintf(" [label=%s]", display_name), ""))]
    
    ## first no clusters: just put them all on lines

    str_nodes_unclustered_parsed <- dt_nodes_labeled[is.na(cluster), paste0(node_parsed, collapse = "; \n")]

    
    ## don't think I can avoid putting this into strings already: one line per cluster
    dt_cluster_parsed <- dt_nodes_labeled[!is.na(cluster)] %>%
        ## filter condition might not be robust
        .[node_parsed == unq_parent, ## adjust parameters of original fake node to make it disappear
          node_parsed := paste0(node_parsed, " [label = \"\", color=transparent, width=0, height=0]")] %>%
        .[, .(cluster_parsed = sprintf("subgraph %s {\n label = \"%s\";\n %s \n}",
                                       cluster[1], # all cluster are the same after grouping by cluster
                                       gsub("cluster_", "", cluster[1]), # just replace "_cluster" as label
                                       paste0(node_parsed, collapse = "; \n"))),
          cluster]

    str_cluster_parsed <- dt_cluster_parsed[, paste0(cluster_parsed, collapse = "\n\n")]
    ## cat(str_cluster_parsed)

    ## parse edges, respecting cluster boundaries
    ## overspecify: specify both ltail and lhead for any edge involved with cluster even if not necessary
    ## then I can do it in one swoop,don't need separate checks
    dt_edges_parsed <- copy(l_gobjs$edges) %>%
        .[, edge_parsed := paste0(called, " -> ", caller,
                                  fifelse(edge_start != called | edge_end != caller,
                                          sprintf("[lhead=%s,ltail=%s]", edge_end, edge_start), ""))]

    str_edges_parsed <- dt_edges_parsed[,  paste0(gsub("\\$", "__", edge_parsed), collapse = "; \n")]
    ## cat(str_edges_parsed)

    graph_parsed <- sprintf("digraph D {\n %s %s %s \n %s \n %s \n %s }",
                            "compound = true;\n splines = true; fontname=helvetica;\n", # graph attributes
                            "node [shape=box, fontsize = 14, fontname=helvetica];\n", # node attributes
                            "edge [style = solid];\n",
                            str_cluster_parsed, str_nodes_unclustered_parsed, str_edges_parsed)
    
    return(graph_parsed)

}




#' generate, write and display the callgraph
#'
#' queries the global environent for the callgraph relations
#' 
#' opens newly constructed callgraph as side effect
#' 
#' uses/requires c_dirs and dpltF
#' @export
gwd_clgrph <- function() {
    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## 1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    

    

    callgraph_parsed <- gl_clgr_objs() %>% parse_clgrph_todot

    writeLines(callgraph_parsed, paste0(c_dirs$misc, "callgraph2.dot"))

    system(sprintf("dot -Tpdf -o %s %s", paste0(c_dirs$figs, "callgraph2.pdf"),
                   paste0(c_dirs$misc, "callgraph2.dot")))

    dpltF("callgraph2")


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
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    input_string <- match.call() %>% deparse %>% paste0(collapse = "")
    pattern <- "mdl_list = list\\(.*?\\)"

    ## Extract the model list argument 
    mdl_names_prep1 <- regmatches(input_string, gregexpr(pattern, input_string)) %>% chuck(1)

    ## yeet the "mdl_list = list"
    c_mdl_names <- substring(mdl_names_prep1, 17, nchar(mdl_names_prep1)-1) %>%
        strsplit(",") %>% unlist %>% trimws

    ## pass custom model names as argument, all other arguments stay
    screenreg(mdl_list, custom.model.names = c_mdl_names, ...)


}

#' recode iso3c to 6 regions (americas split)
#'
#' also puts TWN (Taiwan) into Asia
#' @export 
#' @param iso3cs vector of iso3c 
rcd_iso3c_reg6 <- function(iso3cs) {
    #' custom recoding to PMDB region scheme of 6 continents (NA, LA, EU, AF, AS, OC)
    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## 1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    regs_unreg <- countrycode(iso3cs, "iso3c", "un.region.name",
                              custom_match = c("TWN"= "Asia", "XKX" = "Europe"))
    regs_unregsub <- countrycode(iso3cs ,"iso3c", "un.regionsub.name",
                                 custom_match = c("TWN"= "Asia", "XKX" = "Europe"))

    ## split americas into north and south
    locs_americas <- which(regs_unregsub %in% c("Latin America and the Caribbean", "Northern America"))
    regs_unreg[locs_americas] <- regs_unregsub[locs_americas]

    ## custom recoding to PMDB region names 

    reg6_lbls <<- list(
        EU = "Europe",
        AS = "Asia",
        AF = "Africa",
        NALUL = "North America",
        LA = "Latin America",
        OC = "Oceania")


    reg_cbn_rcd_list <- list(
        Europe = "EU",
        Asia = "AS",
        Africa = "AF",
        "Northern America" = "NALUL", # avoid collision with NA
        "Latin America and the Caribbean" = "LA",
        "Oceania" = "OC")

    rcd_iso3c_reg6_sub <- function(reg, reg_cbn_rcd_list) {
        #' individual handling of reg6 coding: return NA reg not matched
        if (reg %in% names(reg_cbn_rcd_list)) {
            reg_cbn_rcd_list[[reg]]
        } else {
            NA
        }
    }

    map_chr(regs_unreg, ~rcd_iso3c_reg6_sub(.x, reg_cbn_rcd_list))

}

#' uses lsof to see if filename is open
#' mostly used for pdf output
#' @export
check_if_file_is_open <- function(filename) {
    open_check_cmd <- paste0("lsof -w ", filename)
    open_check_res <- suppressWarnings(system(open_check_cmd, intern = T))
    
    ## if an attribute is returned (rather than being null), file is not open
    open <- is.null(attr(open_check_res, "status"))

    return(open)


}


#' open rendered table 
#' @export
dtblF <- function(tblname) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    filename <- paste0(chuck(c_dirs, "tbls"), tblname, ".pdf")

    open <- check_if_file_is_open(filename)

    if (!open) {
        open_cmd <- paste0("zathura ", filename, " &")
        system(open_cmd)
    
    } else if (open) {
        print(sprintf("%s already open", tblname))
    }

}



#' take a table from l_tbls, write it to a tex and (cropped) pdf file
#' @export
wtbl <- function(tblname, c_tbls = do.call("gc_tbls", c_tblargs)) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    filename_tex <- paste0(chuck(c_dirs, "tbls"), tblname, "_wcpF.tex")

    
    
    ## normal process
    ## get table
    tx <- chuck(l_tbls, tblname)
    
    c_scalebox <- ifelse("scalebox" %in% names(tx), chuck(tx, "scalebox"), 1)
    c_tabenv <- ifelse("tabenv" %in% names(tx), chuck(tx, "tabenv"), "tabular")
    c_size <- ifelse("size" %in% names(tx), chuck(tx, "size"), 1)

    ## process the table result into xtable
    tx_procd <- xtable(tx$dt_fmtd,
                       caption = tx$caption,
                       align = tx$align_cfg,
                       label = paste0("tbl:", tblname)) # adjust caption for pandoc compatibility

    ## create config that can be called by print.xtable
    tx_towrite <- list(x=tx_procd, include.rownames = F, include.colnames = F,
                       file = filename_tex,
                       sanitize.text.function = identity,
                       add.to.row = tx$add_to_row,
                       tabular.environment = c_tabenv,
                       size = c_size,
                       hline.after = tx$hline_after)

    if (c_scalebox != 1) {
        tx_towrite <- c(tx_towrite, list(scalebox = c_scalebox))
    }

    if (c_scalebox != 1 & c_tabenv == "longtable") {stop("scalebox and longtable don't go together")}
    
    
    do.call("print.xtable", tx_towrite)

    ## word-compatible export
    ## make cells word-compatible (try latexTranslate), maybe needs more explicity wrapping in math-mode ($)
    ## dt_fmtd_wcpT <- tfmv(tx$dt_fmtd, vars = names(tx$dt_fmtd)[chuck(tx, "number_cols")],
    ##                      FUN = \(x) sprintf("$%s$", x))

    c_number_cols <- names(tx$dt_fmtd)[chuck(tx, "number_cols")]

    dt_fmtd_wcpT <- tx$dt_fmtd %>% copy %>%
        .[, (c_number_cols) := map(.SD, ~sprintf("$%s$", .x)), .SDcols = c_number_cols]

    

    ## update align_cfg (no Dcolumns)
    align_cfg_wcpT <- map(tx$align_cfg, ~fifelse(substring(.x, 1, 1) == "D", "l", .x))

    tx_procd_wcpT <- xtable(dt_fmtd_wcpT, caption = tx$caption, align = align_cfg_wcpT,
                            label = paste0("tbl:", tblname))

    filename_tex_wcpT <- paste0(chuck(c_dirs, "tbls"), tblname, "_wcpT.tex")

    tx_towrite_wcpT <- list(x=tx_procd_wcpT, include.rownames = F, include.colnames = F,
                       file = filename_tex_wcpT,
                       sanitize.text.function = identity,
                       add.to.row = tx$add_to_row,
                       hline.after = tx$hline_after)

    do.call("print.xtable", tx_towrite_wcpT)



    landscape <- F

    if ("landscape" %in% names(tx)) {
        if (tx$landscape) {
            landscape <- T
        }
    }


    ## write-to-pdf the pdf version (i.e.the wcpF version): this is what looks best
    wtbl_pdf(paste0(tblname, "_wcpF"), landscape)

    return(invisible(NULL))

}

#' generate and display table (requires writing to pdf)
#' @param tblname the table to display
#' @return displays table as side effect, if not open already
#' @export
gdtbl <- function(tblname) {

    gtbl(tblname)
    wtbl(tblname)
    dtblF(paste0(tblname, "_wcpF"))
}



## #' write to tex file and render to pdf a gt object
## #' @param tblname the name of the table
## wtbl2 <- function(tblname) {
##     if (as.character(match.call()[[1]]) %in% fstd){browser()}
##     1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    
##     tx <- chuck(l_tbls2, tblname)

##     filename_tex <- paste0(chuck(c_dirs, "tbls"), tblname, ".tex")

##     filecontent_tex <- as_latex(tx) %>% as.character

##     fileConn <- file(filename_tex)
##     writeLines(filecontent_tex, fileConn)
##     close(fileConn)

##     wtbl_pdf(tblname, F)



## }


#' take a tex file, and generate cropped pdf file
#' @param tblname name of table (needs to be in gc_tbls, and thereby in l_tbls)
#' @param landscape whether to use landscape format (rotated A4)
#' @export 
wtbl_pdf <- function(tblname, landscape) {

    ## get the objects to work with 
    ## have some duplication here (tx and filename_tex also in wtbl), but helps to keep function arguments lean
    
    filename_tex <- paste0(chuck(c_dirs, "tbls"), tblname, ".tex")
    filename_pdf <- paste0(chuck(c_dirs, "tbls"), tblname, ".pdf")


    ## copy original table to /tmp
    cmd_copy_table <- sprintf("cp %s %s", filename_tex, paste0("/tmp/", tblname, ".tex"))
    system(cmd_copy_table)

    ## assign landscape: if not set, assume not
    ## also set the texput ending: allows parallel rendering
    texput_ending <- "\\end{document}"
    
    if (landscape) {
        texput_file <- "texput_landscape.tex"
        texput_ending <- paste0("\\end{landscape}", "\n", texput_ending, collapse = "\n")

    } else {
        texput_file <- "texput.tex"
    }


    texputted_filename <- paste0("/tmp/", tblname, "_texput.tex")

    ## for now, copy texput files from dropbox. FIXME: put texput files into package dir
    cmd_copy_texput <- sprintf("cp /home/johannes/Dropbox/phd/jtls/inst/%s %s",
                       texput_file, texputted_filename)
    system(cmd_copy_texput)
    
    ## add the table name to the texputted file
    texput_input <- sprintf("\\input{%s}\n%s", paste0(tblname, ".tex"), texput_ending)
    cmd_finalize_texput <- sprintf("echo \"%s\" >> %s", texput_input, texputted_filename)
    
    system(cmd_finalize_texput)

    ## actual compile command
    cmd_compile <- paste0("cd /tmp && pdflatex ", texputted_filename)
    system(cmd_compile)
    
    ## crop and copy cropped file back to table dir
    crop_cmd <- sprintf("cd /tmp && pdfcrop %s %s",
                        paste0("/tmp/", tblname, "_texput.pdf"), # the compiled pdf file in /tmp
                        filename_pdf) # the target pdf in c_dirs$tbls
    
    system(crop_cmd)

    return(invisible(NULL))

}
   
#' generate a table: put it into l_tbls
#' @export
gtbl <- function(tblname, c_tbls = do.call("gc_tbls", c_tblargs)) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    c_tbl <- chuck(c_tbls, tblname)
    ## yeet caption
    c_tbl_nocap <- c_tbl %>% .[names(.) %!in% "caption"]
    
    ## generate the list with the components that I need for xtable plotting
    tx <- do.call(paste0("g", tblname), c_tbl_nocap)

    ## re-add caption 
    tx2 <- c(tx, list(caption = chuck(c_tbl, "caption")))
    
    l_tbls[[tblname]] <<- tx2

    return(invisible(TRUE))    


}

#' generate the in-text references to tables
#' to be used in gd_nbrs
#' ynktbl not possible because "#+include:" commands are solved before macros are expanded
#' @return data.table with nbr_name, nbr_fmt, grp columns
#' @export
gc_reftbl <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    tblnames <- do.call("gc_tbls", list()) %>% names()

    data.table(tblname = tblnames) %>%
        .[, macro := sprintf("\\ref{tbl:%s}", tblname)] %>%
        .[, .(nbr_name = paste0("reftbl_", tblname),
              nbr_fmt = macro,
              grp = "reftbl")]

}


#' generates stars depending on p-value for latex
#' @param p the p-value
#' @return latex star string
#' @export
fmt_pvlu <- function(p) {
    if (p >= 0.05) stars <- ""
    if (p < 0.05) stars <- "^{*}"
    if (p < 0.01) stars <- "^{**}"
    if (p < 0.001) stars <- "^{***}"
    stars
}

#' format a "cell" in a regression table
#' processes numbers into strings (easier to work with text-wise)
#' not sure if this great idea..
#' some other package could handle (each element like coef, se, pvalue) separately -> more flexibility
#' @param coef the main number
#' @param se standard error
#' @param pvalue the pvalue: for now used for stars
#' @param wcptbl whether table has to work in word: then use math mode
#' @param type how to format the cell
#' @return formatted cell string
#' @export 
fmt_cell <- function(coef, se, pvalue, type) {
    ## wcptbl

    if (type == "coef-se-stars") {

        cell_proc <- sprintf("%s \\; (%s)%s", # \\; 
                             coeftostring(coef, lead.zero = F, digits = 2),
                             format(round(se,2), nsmall = 2),
                             fmt_pvlu(pvalue))
    } else if (type == "coef-stars") {

        cell_proc <- sprintf("%s%s",
                             coeftostring(coef, lead.zero = F, digits = 2),
                             fmt_pvlu(pvalue))
    }

    ## if mswcptbl = T, wrap cell contents in $ for math mode
    ## if (wcptbl) cell_proc <- sprintf("$%s$", cell_proc)

    return(cell_proc)

}


#' generate the multicolumn latex commands for variable groups
#' uses the grouping label (rather than "raw" grouping id) to save a merge step
#' to use in add_to_row
#' @param dtx  the (ordered) data.frame, needs to have the grouping variable
#' @param grp the variable to group by, should already be the grp_label
#' @export
#' @return data.table with columns grpstr (the group latex string),  pos (its position) grp_intern (grp vrbl)
gc_grpstrs <- function(dtx, grp, nbr_cols, bold = T) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
   
    

    dtx <- copy(dtx)[, nbr := 1:.N] %>%
        .[, grp_intern := get(grp)] %>% # set up separate variable to still have NSE even when variable changes
        .[, .(pos = min(nbr)-1), grp_intern]

    if (bold) {
        dtx[, grpstr := sprintf("\\multicolumn{%s}{l}{\\textbf{%s}} \\\\ \n", nbr_cols, grp_intern)]
    } else {
        dtx[, grpstr := sprintf("\\multicolumn{%s}{l}{%s} \\\\ \n", nbr_cols, grp_intern)]
    }
        
}


#' generate note of pvalues/significance
#' used in new table framework
#' @param se_mention whether "standard errors in parantheses" is to be included
#' @param ncol number of columns for multicolumn
#' @export
#' @return string of significance note
gc_signote <- function(se_mention, ncol) {
      
    se_note <- "standard errors in parantheses."

    ## generate significance note
    sig_note_vlus <- paste0(fifelse(se_mention, se_note, ""),
                            "\\textsuperscript{***}p $<$ 0.001;",
                            "\\textsuperscript{**}p $<$ 0.01;",
                            "\\textsuperscript{*}p $<$ 0.05.")

    sig_note <- sprintf("\\hline \n \\multicolumn{%s}{l}{\\footnotesize{%s}} \\\\ \n", ncol, sig_note_vlus)

    return(sig_note)    

}

#' generate spanner for grouping columns
#'
#' uses multicolumn to generate additional line
#' @param spanner_lbls vector of labels for the spanner text
#' @param spanner_lengths vector of column lengths for each spanner
#' @export
#' @return string with multicolumn-spanner information, to be added to at_to_row
gc_spanner <- function(spanner_lbls, spanner_lengths) {
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    lbls_spanner <- map2(spanner_lbls, spanner_lengths, ~sprintf("\\multicolumn{%s}{c}{%s}", .y, .x)) %>%
        paste0(collapse = " & ")

    ## cmidrules <- map2(spanner_lengths, cumsum(spanner_lengths),
    ##                   ~ifelse(.x > 1, sprintf("\\cmidrule(r){%s-%s}", .y - .x + 1, .y), "") %>%

    cmidrules <- pmap(list(splen = spanner_lengths, spcumlen = cumsum(spanner_lengths), lbl = spanner_lbls),
                       \(splen, spcumlen, lbl)
                       ifelse(trimws(lbl) != "", sprintf("\\cmidrule(r){%s-%s}", spcumlen - splen + 1, spcumlen),
                              "")) %>%
        paste0(collapse = "")

    spanner <- paste0(lbls_spanner, " \\\\ \n", cmidrules)

    return(spanner)
    

}

#' generates the column labels/headers with multicolumn
#'
#' for now no support for spanners, but I guess they can be separately generated
#' @param colnames vector of the columns in the order they appear in the table
#' @param collbs named vector with colname as key, and column label as value
#' @param hline_above flag whether to put hlines before the column names, default TRUE. can be switched off when using a multi-line header (spanner), in that case the hlines have to be provided by that or manually otherwise.
#' @param align how to align multicolumns: either single character (then gets recycled) or vector (for separate alignment)
#' @return formatted string, to be added to add_to_row
#' @export
gc_colnames <- function(col_names, col_lbls, hline_above = T, align = "l") {

    c_colnames_fmtd <- purrr::map2(col_names, align,
                           ~sprintf("\\multicolumn{1}{%s}{%s}", .y, latexTranslate(chuck(col_lbls, .x)))) %>%
        paste0(collapse = " & ")

    if (hline_above) {

        c_colnames <- paste0("\\hline \n ", c_colnames_fmtd, "\\\\ \n") ## add hline and linebreaks
    } else {
        c_colnames <- paste0(c_colnames_fmtd, "\\\\ \n")
    }
    

    return(c_colnames)
    
}

#' tidy coxph regression model into dt
#' needs: vrbl, mdl_name, coef, se, pvalue
#' @param rx a coxph model, if unit_name is used the data should come from a data.table, and be accessible in the global environment
#' @param mdl_name model name
#' @param unit_name the name of the unit contributing the times at risk, e.g. individual/organization
#' @export
#' @return list of dt_coef and dt_gof
gd_reg_coxph <- function(rx, mdl_name, unit_name = NULL) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    ## browser()
    
    l_gof_vrbls <- c("nobs", "nevent", "logLik", "AIC", "BIC", "mdl_name", "df")

    ## maybe using term is indeed better than variable, better way of dealing with categorical variables
    dt_coef <- broom::tidy(rx) %>% adt() %>%
        .[, .(term, coef = estimate, se = std.error, pvalue= p.value, mdl_name = mdl_name)]
    
    dt_gof_prep <- broom::glance(rx) %>% adt %>%
        .[, mdl_name := mdl_name]

    dt_gof_prep$df <- summary(rx)$waldtest[[2]]

    ## parse model name to get number of units (e.g. individuals/organizations)
    if (!is.null(unit_name)) {

        dt_mdl_str <- as.list(rx$call)$data
        dt_mdl <- eval(parse(text = deparse(dt_mdl_str)))

        if ("data.table" %!in% class(dt_mdl)) {
            stop("coxph model data source is not a data.table")
        }
        
        ## print(dt_mdl)
        nunits <- dt_mdl[, uniqueN(get(unit_name))]

        dt_gof_prep$nunits <- nunits ## add nunits to dt_gof_prep
        l_gof_vrbls <- c("nunits", l_gof_vrbls) ## add nunits call to l_gof_vrbls

    }
    
    dt_gof <- dt_gof_prep[, .SD, .SDcols = l_gof_vrbls] # get the gof values
    

    ## apply(dt_gof, 2, typeof)
    ## sapply(dt_gof, typeof)

    return(list(
        dt_coef = dt_coef,
        dt_gof = dt_gof))


}

#' generate a custom_table object
#'
#' dt_coef: can come from whereever, 
#' dt_vrblinfo, dt_ctgterm_lbls and dt_gof_cfg are generally provided by gc_vvs() in main project
#' @param dt_coef dt with vrbl, mdl_name, coef, se, pvalue
#' @param dt_gof wide dt with GOF stats and mdl_name
#' @param dt_vrblinfo dt with info on variables: vrbl, vrbl_lbl, vrblgrp, vrblgrp_lbl
#' @param dt_ctgterm_lbls dt with info on terms of categorical variables: vrbl, term, term_lbl
#' @param dt_gof_cfg dt with info on GOF: gof_name, digits (for founding), gof_lbl
#' @param mdl_lbls vector of model labels (keys are model names)
#' @return custom table framework
#' @export
gt_reg <- function(dt_coef, dt_gof, dt_vrblinfo, dt_ctgterm_lbls, dt_gof_cfg, mdl_lbls) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
        
    ## FIXME: add logic for when there are no categorical variables
    ## FIXME: add logic for when no groups

    ## -------- SECTION COEF ----------

    ## first combine continuous and categorical variables
    ## add variable group to categorical variables
    ## use update join for categorical terms, then rbind with variable info (continuous variables)
    dt_termlbls <- copy(dt_ctgterm_lbls)[dt_vrblinfo,
                                         `:=`("vrblgrp" = i.vrblgrp, "vrblgrp_lbl" = i.vrblgrp_lbl),
                                         on = "vrbl"] %>%
        .[, .(term, vrbl, term_lbl, vrblgrp, vrblgrp_lbl)] %>% 
        rbind(dt_vrblinfo[, .(term = vrbl, vrbl, term_lbl = vrbl_lbl, vrblgrp, vrblgrp_lbl)])
        
    ## merge with dt_coefs, format cells, cast into wide, order
    dt_coef_wide_prep <- dt_termlbls[dt_coef, on = "term"] %>%
        .[, cell_fmt := fmt_cell(coef, se, pvalue, type = "coef-se-stars"), 1:nrow(.)] %>%
        dcast(term_lbl + term + vrblgrp + vrblgrp_lbl ~ mdl_name, value.var = "cell_fmt") %>% 
        .[, term := factor(term, levels = levels(dt_termlbls$term))] %>% # re-add term factor order
        .[order(vrblgrp, term)]
        
    ## select columns
    dt_coef_wide <- dt_coef_wide_prep[, c("term_lbl", unique(dt_coef$mdl_name)), with = F] %>%
        cbind(grp_filler = "", .) %>%
        .[, term_lbl := latexTranslate(term_lbl)]

    ## ------- SECTION DECORATIONS -------


    dt_grpstrs <- gc_grpstrs(dt_coef_wide_prep, grp = "vrblgrp_lbl", length(mdl_lbls) + 2) # generate group strings
    ## make them over all the columns (grp_filler, vrbl, number of models)

    signote <- gc_signote(se_mention = T,ncol = length(mdl_lbls) + 2) # generate signote

    c_colnames <- gc_colnames(names(dt_coef_wide), col_lbls = # generate column names
                                                 c(list(grp_filler = "", term_lbl = "Variable"), mdl_lbls))
    
    ## ------ SECTION GOF -----------
        
    ## melt gofs, format them 
    dt_gof_long <- suppressWarnings(melt(dt_gof, id.vars = "mdl_name",
                          variable.name = "gof_name", value.name = "gof_value")) %>%
        dt_gof_cfg[., on = "gof_name"] %>%
        .[, gof_fmt := format(gof_value, digits = max(digits,1), nsmall = digits, scientific = F), .I] %>%
        .[, gof_lbl := latexTranslate(gof_lbl)]

    ## cast gof into wide, order doesn't matter for rbind
    dt_gof_wide <- dcast(dt_gof_long, gof_name + gof_lbl ~ mdl_name, value.var = "gof_fmt") %>%
        .[, `:=`(term_lbl = gof_lbl, gof_name = NULL, gof_lbl = NULL, grp_filler = "")]


    ## -------- SECTION COMBINE EVERYTHING -------

    ## combine coefs with gof
    dt_viz <- rbind(dt_coef_wide, dt_gof_wide)

                                                  
    c_atr <- list(
        pos = c(list(-1, nrow(dt_viz)), dt_grpstrs$pos),
        command = c(c_colnames, signote, dt_grpstrs$grpstr))

    list(dt_fmtd = dt_viz,
         align_cfg = c("l", "p{0mm}", "l", rep("D{)}{)}{8)3}",length(mdl_lbls))), 
         hline_after = c(-1, nrow(dt_coef_wide)), # lines at top and before gof block
         add_to_row = c_atr,
         number_cols = c(rep(F,2), rep(T,length(mdl_lbls))))


}

#' generate a plain summary table: just pass as is, with proper rounding and formatting
#'
#' @param dt_plain table to print
#' @param b_landscape bool whether to use landscape
#' @return a jtbl 
#' @export
gt_plain <- function(dt_plain, b_landscape = F) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    
    l_b_numvrbls <- sapply(dt_plain, is.numeric) # bool whether a variable is numeric
    l_numvrbls <- names(dt_plain)[l_b_numvrbls] # list of numeric variables
    l_charvrbls <- names(dt_plain)[!l_b_numvrbls] # list of character variablesg

    
    if (length(l_numvrbls) > 0) {
            l_numvrbls_chr <- paste0(l_numvrbls, "_chr")
    } else {
        l_numvrbls_chr <- character(0)
    }

    # l_numvrbls_chr <- ifelse(l_numvrbls), paste0(l_numvrbls, "_chr"), NULL) # intermediate group for num -> char conversion
    l_vrbls <- names(dt_plain) # all ze names

    ## convert 
    dt_fmtd <- dt_plain %>% copy %>%
        .[, (l_numvrbls_chr) := map(.SD, ~format(.x, digits = 3, big.mark = " ",
                                                 nsmall = fifelse(.x > 100, 0, 1))),
          .I, .SDcols = l_numvrbls] %>%
        .[, (l_numvrbls) := NULL] %>%
        setnames(l_numvrbls_chr, l_numvrbls) %>%
        .[, (l_charvrbls) := map(.SD, latexTranslate), .SDcols = l_charvrbls] %>% # ensure latex
        .[, .SD, .SDcols = l_vrbls]

    c_colnames <- names(dt_fmtd) %>% gc_colnames(., setNames(.,.))

    c_atr <- list(
        pos = list(-1),
        command = c_colnames)

    list(dt_fmtd = dt_fmtd,
         ## align_cfg = c("l", "l", rep("r", 4), rep("l", 3)),
         align_cfg = c("l", ifelse(l_b_numvrbls, "r", "l")),
         hline_after = c(0, nrow(dt_plain)),
         add_to_row = c_atr,
         ## number_cols = c(F, rep(T, 4)),
         number_cols = l_b_numvrbls,
         landscape = b_landscape)
    
}



#' generate information on within and between variation, similar to Stata's `xtsum` command
#' 
#' @param data the data 
#' @param varname variable to calculate the variation for (don't quote it)
#' @param unit variable indicating groups (don't quote it)
#' @export 
xtsum <- function(data, varname, unit) {
    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    ## Xtsum
    varname <- enquo(varname)
    loc.unit <- enquo(unit)
    ores <- data %>% summarise(Mean=mean(!! varname, na.rm=TRUE), sd=sd(!! varname, na.rm=TRUE), min = min(!! varname, na.rm=TRUE), max=max(!! varname, na.rm=TRUE), N=sum(as.numeric((!is.na(!! varname)))))
    bmeans <- data %>% group_by(!! loc.unit) %>% summarise(meanx=mean(!! varname, na.rm=T), t.count=sum(as.numeric(!is.na(!! varname)))) 
    bres <- bmeans %>% ungroup() %>% summarise(sd = sd(meanx, na.rm=TRUE), min = min(meanx, na.rm=TRUE), max=max(meanx, na.rm=TRUE), n=sum(as.numeric(!is.na(t.count))), `T-bar`=mean(t.count, na.rm=TRUE))
    wdat <- data %>% group_by(!! loc.unit) %>% mutate(W.x = scale(!! varname, center=TRUE, scale=FALSE))
    wres <- wdat %>% ungroup() %>% summarise(sd=sd(W.x, na.rm=TRUE), min=min(W.x, na.rm=TRUE), max=max(W.x, na.rm=TRUE))
    ## Loop to adjust the scales within group outputs against the overall mean
    for(i in 2:3) {
        wres[i] <- sum(ores[1], wres[i])
    }

    ## Table Output
    ## Variable <- matrix(c(varname, "", ""), ncol=1)
    ## Variable <- matrix(c("varname", "", ""), ncol=1)
    Variable <- matrix(c(quo_name(varname), "", ""), ncol=1)
    Comparison <- matrix(c("Overall", "Between", "Within"), ncol=1)
    Mean <- matrix(c(ores[1], "", ""), ncol=1)
    Observations <- matrix(c(paste0("N = ", ores[5]), paste0("n = ", bres[4]), paste0("T-bar = ", round(bres[5], 4))), ncol=1)
    Tab <- rbind(ores[2:4], bres[1:3], wres[1:3])
    Tab <- cbind(Tab, Observations)
    Tab <- cbind(Mean, Tab)
    Tab <- cbind(Comparison, Tab)
    Tab <- cbind(Variable, Tab)

    ## Output
    return(Tab)
}


#' prepare (and optionally move) a data.frame to a sqlite DB
#'
#' removes existing tables called table_title
#' @param dbx sqlite database connection
#' @param dfx data.frame to insert
#' @param table_title name of table
#' @param constraints list of constraints to pass to CREATE table command
#' @param insert_data whether to actually insert data (default F)
#' 
#' @export
#' 
prep_sqlitedb <- function(dbx, dfx, table_title, constraints, insert_data = F) {
    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    ## create schema for dfx (in terms of dbx)
    ## add any additional constraints (primary key(s), foreign keys)
    ## insert data if insert_data==T
    
    schema <- dbDataType(dbx, dfx)

    if (table_title %in% dbListTables(dbx)) {
        dbRemoveTable(dbx, table_title)
    }

    ## try to modify primary keys: modify schema
        
    init_part <- sprintf("CREATE TABLE %s (", table_title)

    
    column_info <- paste0(imap_chr(schema, ~sprintf("%s %s", .y, .x)))
    ## constraints <- "PRIMARY KEY (mdl_id, gof_names)"
        
    setup_cmd <- paste0(c(init_part,
                          paste(c(column_info, constraints), collapse = ",\n"), ")"), collapse = "\n")
    cat(setup_cmd)
    dbSendQuery(dbx, setup_cmd)

    if (insert_data) {
        dbAppendTable(dbx, table_title, dfx)
    }
    return(invisible(T))
}


## ----------- automatic callgraph

#' see which functions are defined in srcfiles, these should be tracked
#'
#' @param l_srcfiles list of sourcefiles where functions are defined
#' @return list of (user-defined) functions to be tracked
gl_funcs_to_track <- function(l_srcfiles) {

    ## browser()
    ## get all functions defined globally
    l_global_funcs <- c(lsf.str(globalenv())) %>% setNames(.,.)
    lapply(l_global_funcs, \(x) print(sprintf("global func: %s", x)))

    ## getSrcFilename(gd_mtcars_filtered)

    ## l_funcs_wfile <- lapply(l_global_funcs, \(x) getSrcFilename(get(x)))
    ## getSrcFilename(gd_mtcars_base)


    l_funcs_wfile <- lapply(l_global_funcs, \(f) do.call(getSrcFilename, list(x=f), envir = globalenv()))
    
    ## l_srcfiles <- c("ac_funcs.R")
    ## see where they are defined
    ## l_funcs_wfile <- imap(l_global_funcs, ~getSrcFilename(get(.x)))
    ## l_funcs_wfile <- imap(l_global_funcs, ~getSrcFilename(get(.x)))
    lapply(1:10, \(x) print(sprintf("number with file: %s", x)))
    lapply(l_funcs_wfile, \(x) print(sprintf("func with file: %s", x)))
    

    ## keep those that are defined in the source files
    l_funcs_filtered <- keep(l_funcs_wfile, ~len(.x) != 0) %>% # first filter out those where file is unclear
        keep(~any(sapply(l_srcfiles, \(f) grepl(.x, f))))    

    lapply(l_funcs_filtered, \(x) print(sprintf("func filtered: %s", x)))

}


#' add tracking code to a function: track input arguments and give output a gnrtdby attribute
#'
#' @param fx a function
#' @param fname the name of the function (passing that too makes debugging easier)
track_function <- function(fx, fname) {
    
    ## original function parameters
    bx <- body(fx)
    len_func <- length(bx)

    ## keep track of how many statements have been added
    gwfargs_added <- 0
    gnrtdby_added <- 0

    ## add the gw_fargs call, but only if there are no arguments
    if (length(formals(fx)) > 0) {

        ## shift statements down to enter gw_fargs at the top
        for (i in length(body(fx)):2) { # :2 don't shift top statement (curly braces)
            body(fx)[[i+1]] <- body(fx)[[i]]
        }

        body(fx)[[2]] <- substitute(gw_fargs(match.call()))
        gwfargs_added <- 1
    }

    ## add gnrtdby attribute, unless functions returns plots or tables
    if (substring(fname, 1,3) %!in% c("gp_", "gt_", "gc_")) {

        if (!any(as.character(bx[[len_func]]) == "return")) {
            stop(sprintf("%s: final argument does not contain return statement", fname))
        }

        ## add attributed generated before return statement
        ret_obj <- as.character(body(fx)[[len_func + gwfargs_added]])[2] # first get object which is returned
        body(fx)[[len_func + gwfargs_added + 1]] <- body(fx)[[len_func + gwfargs_added]] # add another return statement
        
        ## generate the generated_by expression
        gnrtd_by_expr <- str2lang(sprintf("attr(%s, \"gnrtdby\") <- as.character(match.call()[[1]])", ret_obj))
        ## overwrite the first return statement with expression
        body(fx)[[len_func + gwfargs_added]] <- gnrtd_by_expr

        gnrtdby_added <- 1

    }

    print(sprintf("%s, gwfargs: %s, gnrtdby: %s", fname, gwfargs_added, gnrtdby_added))

    ## overwrite original function
    assign(fname, fx, envir = globalenv())

    return(invisible(TRUE))
}

#' track all functions in the sourcefiles
#'
#' @param l_sourcefiles: list of files that contain functions
#' @export 
track_sourcefiles <- function(l_sourcefiles) {
    ## get functions to track
    l_funcs_to_track <- gl_funcs_to_track(l_sourcefiles) %>% names
    lapply(l_funcs_to_track, \(x) print(sprintf("func to track %s", x)))

    ## track them
    lapply(l_funcs_to_track, \(x) track_function(get(x), x))
    return(invisible(TRUE))
}



#' filter a dt based on grepl and all columns, intended for interactive use with r-filter-dt,
#' inspired by fzf
#'
#' @param df data.frame to filter
#' @param query some search term, can be multiple (split on space)
#' @param filter_state how query is applied to columns
#' @param l_cols list of columns to display
#' @param verbose whether to print, used for debugging
#' @export
filter_dt <- function(df, query, filter_state, l_cols, verbose = F) {


    ## filter down rows of data table based on query match
    print(paste0(query, " - ", filter_state))

    dtx <- as.data.table(df)
    
    ## if no colums are specified, use all
    if (is.null(l_cols)) {
        l_cols <- names(dtx)
    }
    ## split query terms on spae
    l_query_split <- unlist(strsplit(query, " "))

    ## construct basic mask: check which rows are met
    dt_mask <- map(char_vars(dtx, return = "names"),
        ~dtx[, pall(lapply(l_query_split, \(term) grepl(term, get(.x), ignore.case = T)))]) %>%
        Reduce(\(x,y) cbind(x,y), .) %>% adt
        
    ## print(l_cols)
    
    dtx[rowSums(dt_mask) > 0, .SD, .SDcols = l_cols] %>% print.data.table
    
}


#' return largest objects in global environment
#' 
#' @export
gd_objsize <- function() {

    dt_objsize <- data.table(obj = ls(envir = globalenv())) %>%
        .[, size := object.size(get(obj)), .I] %>%
        .[, size_fmt := format(size, units = "GB")] %>% 
        .[order(-size)]

    
    
    print(sprintf("total size: %s GB", dt_objsize[, round(sum(size)/1024^3, 3)]))

    return(dt_objsize)
    
}


#' show pass secrets
#'
#' @export
show_pass_secret <- function(secret) {
    system(sprintf("pass show %s", secret), intern = T)
}




#' create sqlite json db
#' @param dbname file to sqlite db
#' @param container_key sqlite container key
#' @export
nodb_creator <- function(dbname, container_key) {
    src <- src_sqlite(dbname)

    docdb_create(src, container_key, value = NULL)
}

#' inserting json rows into sqlitedb
#' @param dbname filepath for sqlite db
#' @param container_key sqlite container key
#' @param data data to insert
nodb_inserter <- function(dbname, container_key, data) {
    src <- src_sqlite(dbname)

    docdb_create(src, container_key, data)
}


#' geocode a chunk of a data.table
#' requires that gc_geocode_cfg is defined somewhere: needs to return a list of the elements to the geocode function
#'
#' @param data_to_geocode_chunk data.table with some rows to be geocoded
#' @param container_ke sqlite container key
#' @param dbname filepath for sqlite db
gwd_geocode_chunker <- function(data_to_geocode_chunk, container_key, dbname) {
    #' geocode a chunk of data and insert it to the database
    Sys.sleep(0.5)
    ## get geocode settings and merge with actual geocoded data
    l_args <- gc_geocode_cfg() %>% chuck(container_key) %>% c(list(.tbl = data_to_geocode_chunk))
    
    ## actual geocoding
    dt_geocoded <- do.call(geocode, l_args)

    col_addr <- chuck(l_args, "address")

    ## merge IDs back: addr gets returneda as address %>% can return it
    dt_geocoded_wid <- merge(dt_geocoded, data_to_geocode_chunk[, .(ID, address = get(col_addr))], on = "address")

    
    print(dt_geocoded_wid)
    nodb_inserter(dbname, container_key, dt_geocoded_wid)

    return(invisible(T))

}


#' geocode function: check which stuff has been geocoded, then geocode the rest
#' uses chunking to be restartable
#' write results to sqlite json for later flattening
#'
#' @param dt_to_geocode data.table with data
#' @param container_key sqlite container key
#' @param dbname filepath to sqlitedb
#' @export 
gwd_geocode <- function(dt_to_geocode, container_key, dbname) {

    ## get existing data
    src_nosql <- src_sqlite(dbname)
    src_sql <- dbConnect(SQLite(), dbname)

    ## check which IDs are already there
    cmd_IDs_present <- sprintf("select %s.json ->> 'ID' as ID from %s", container_key, container_key)
    dt_IDs_present <- dbGetQuery(src_sql, cmd_IDs_present) %>% adt
    dt_to_geocode_filtered <- dt_to_geocode[!dt_IDs_present, on = "ID"]

    print(sprintf("data size: %s, already coded: %s, left to code: %s",
                  dt_to_geocode[, .N], dt_IDs_present[, .N], dt_to_geocode_filtered[, .N]))

    ## split into chunks
    l_dt_to_geocode <- split(dt_to_geocode_filtered, 1:dt_to_geocode_filtered[, (.N/5)])

    map(l_dt_to_geocode, gwd_geocode_chunker, container_key, dbname)
    
    ## get arguments
    
    return(invisible(T))
}



#' turn geocoded json table into a flat table
#' for now makes a bunch of simplifying assupmtions:
#' 1. only first line by ID
#' 2. only columns that can be modelled as flat vectors (no lists, listed dfs etc)
#' maybe will have time to deal with all those later on
#'
#' @export
gwd_flat_geocode <- function(container_key, db_name) {
    
    
    src <- src_sqlite(db_name)
    src2 <- dbConnect(SQLite(), db_name)

    dt_json <- docdb_get(src, key = container_key) %>% adt

    l_vrlbs_tokeep <- sapply(dt_json, class) %>% keep(~.x %!in% c("list")) %>% names
    
    ## dt_json[, .SD[nrow(.SD) > 1], ID][, .(ID, score)] %>% print(n=80)
    ## dt_json[ID == 51370, .(coords = sprintf("%s %s", lat, long), address, score)]
    
    dt_to_insert <- dt_json[, head(.SD, 1), ID, .SDcols = setdiff(l_vrlbs_tokeep, "ID")]
    
    ## cmd_setup <- sprintf("CREATE TABLE %s (%s)"
    ##                      col_prefix, dbDataType(dbx, id_vlu), col_prefix,l_schemas[1])

    name_flat_table <- paste0(container_key, "_flat")

    if (dbExistsTable(src2, name_flat_table)) dbExecute(src2, sprintf("drop table %s", name_flat_table))
    
    dbCreateTable(src2, name = name_flat_table, dt_to_insert)
    dbAppendTable(src2, name = name_flat_table, dt_to_insert)

    
}

#' vector to data.table helper function
#'
#' @param v vector
#' @param name name for name column
#' @param vlu name for value column
#' @export
vadt <- function(v, name = "name", vlu = "vlu") {
        dtx <- data.table(namex = names(v), vlux = v) %>%
            setnames(old = c("namex", "vlux"), new = c(name, vlu))
        return(dtx)
}

#' 2d index to 1d index
#' used for dist matrixes
#' @param i row index
#' @param j col index
#' @param dist_obj distance matrix
#' @export
i2d1d <- function (i, j, dist_obj) {
    if (!inherits(dist_obj, "dist")) stop("please provide a 'dist' object")
    n <- attr(dist_obj, "Size")
    valid <- (i >= 1) & (j >= 1) & (i > j) & (i <= n) & (j <= n)
    k <- (2 * n - j) * (j - 1) / 2 + (i - j)
    k[!valid] <- NA_real_
    k
}

#' 1D index to 2D index, used to get locations in distance matrix
#' @param k 1d index
#' @param dist_obj distance matrix
#' @return two column matrices of indices, or labels if they exist
#' @export
i1d2d <- function (k, dist_obj) {
    ## https://stackoverflow.com/questions/39005958/r-how-to-get-row-column-subscripts-
    ## of-matched-elements-from-a-distance-matri
    
    if (!inherits(dist_obj, "dist")) stop("please provide a 'dist' object")
    n <- attr(dist_obj, "Size")
    valid <- (k >= 1) & (k <= n * (n - 1) / 2)
    k_valid <- k[valid]
    j <- rep.int(NA_real_, length(k))
    j[valid] <- floor(((2 * n + 1) - sqrt((2 * n - 1) ^ 2 - 8 * (k_valid - 1))) / 2)
    i <- j + k - (2 * n - j) * (j - 1) / 2

    ## if matrix has labels, use them (they are char tho)
    if ("Labels" %in% names(attributes(dist_obj))) {
        lbls <- attributes(dist_obj)$Labels
        cbind(lbls[i], lbls[j])
    } else {
        cbind(i, j)
    }
}
