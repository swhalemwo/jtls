
#' @import magrittr
#' @import graph
#' @import data.table
#' @importFrom mvbutils foodweb
#' @importFrom xtable xtable print.xtable
#' @importFrom texreg coeftostring
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

    fwrite(dt_fargs, paste0(c_dirs$misc, "farg_calls.csv"), append = T)
}



#' generate function links with mvbutils::foodweb
#'
#' @export
#' @return list with dataframes for edges and nodes
gl_funlinks <- function() {
  
    fw <- mvbutils::foodweb(plotting = F, where = 1) # funs = ls())


    funcs_to_yeet <- .c(atb, adt,len, print.data.table, print_data_table, adf, achr, anum, gw_fargs, gl_funlinks,
                        gl_funmat, gg_clgrph, gc_dirs, gc_plts, gd_nbrs, gl_clgr_objs, gc_clgrphattrs,
                        gc_tbls)

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
        
    ## reconstruct full item paths
    subitem_fullpaths <- paste0(fifelse("$" %in% item_name, parent, item_name), "$", subitem_names_all)
    
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
    if (len(prevlinks) == 0) {
        prevlinks <- flatlist

    } else {
        ## only append items when there are items to append
        if (len(flatlist) > 0) {
            prevlinks <- c(prevlinks, flatlist)
        }
    }
            
    if (len(subitem_names_rel) > 0) {
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

    if (len(l_objs_gnrtdby) > 0) {

        ## get the function that global objects are have been generated by by querying their classes
        dt_gnrtdby <- l_objs_gnrtdby %>% 
            map(~list(obj_name = .x, gnrtdby = attr(get(.x), "gnrtdby"))) %>% rbindlist %>%
            .[, .(caller = obj_name, called = gnrtdby)] %>%
            .[, `:=`(linktype = "obj-fun")]

        ## get the relationships of parents to children to assign children/subitems to their parents
        ## for children that have gnrtdby attribute (as long as their parents have it too), 
        
        ## these are not links, these are the subgraphs -> don't bind them with the other links
        dt_parchild_rels <- map(l_objs_gnrtdby, ~rbindlist(rec_attr_query(.x))) %>% list_rbind %>% funique %>%
            .[, linktype := "par-child"]
        

    } else if (len(l_objs_gnrtdby) == 0) {
        dt_gnrtdby <- data.table(caller = character(), called = character(), linktype = character(),
                                 display_name = character())
    }
        

    ## evaluate the farg_calls file (input arguments)
    dt_fargs <- fread(paste0(c_dirs$misc, "farg_calls.csv"))[, .(caller = fname, called = param_value)] %>%
        unique %>% 
        .[is.na(as.logical(called))] %>% # filter out logical switches
        .[, linktype := "fun-obj"]

    ## combine into object-funtion links
    dt_obfn_links <- rbind(dt_gnrtdby, dt_fargs)

    l_funlinks <- gl_funlinks()

    ## best way to store clusters: in the nodes dt?
    ## can also update display name there
    dt_nodes <- data.table(node = unique(c(l_funlinks$nodes$fun, dt_obfn_links$called,
                                           dt_obfn_links$caller, dt_parchild_rels$item))) %>% 
        dt_parchild_rels[, .(item, parent, display_name)][., on = .(item = node)]

    ## need to create the fake cluster nodes: to get 
    dt_clusters <- dt_nodes[!is.na(parent), .(unq_parent = funique(parent))] %>%
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
                            "compound = true;\n splines = false; fontname=helvetica;\n", # graph attributes
                            "node [shape=box, fontsize = 14, fontname=helvetica];\n", # node attributes
                            "edge [style = solid];\n",
                            str_cluster_parsed, str_nodes_unclustered_parsed, str_edges_parsed)
    
    return(graph_parsed)

}




#' generate, write and display the callgraph
#'
#' queries the global environent for the callgraph relations
#' opens newly constructed callgraph as side effect
#' uses/requires c_dirs and dpltF
#' @export
gwd_clgrph <- function() {
    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## 1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## old version based on Rgraphviz
    ## px <- gl_clgr_objs() %>% gg_clgrph

    ## pdf(paste0(c_dirs$figs, "callgraph.pdf"), height = 3, width = 6)
    ## renderGraph(layoutGraph(px, attrs = gc_clgrphattrs()))
    ## dev.off()

    

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

    regs_unreg <- countrycode(iso3cs, "iso3c", "un.region.name", custom_match = c("TWN"= "Asia"))
    regs_unregsub <- countrycode(iso3cs ,"iso3c", "un.regionsub.name", custom_match = c("TWN"= "Asia"))

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
 
    filename_tex <- paste0(chuck(c_dirs, "tbls"), tblname, ".tex")
    

    ## get table
    tx <- chuck(l_tbls, tblname)
 
    ## process the table result into xtable
    tx_procd <- xtable(tx$dt_fmtd, caption = tx$caption, align = tx$align_cfg,
                       label = paste0("tbl:", tblname)) # adjust caption for pandoc compatibility

    ## create config that can be called by print.xtable
    tx_towrite <- list(x=tx_procd, include.rownames = F, include.colnames = F,
                       file = filename_tex,
                       sanitize.text.function = identity,
                       add.to.row = tx$add_to_row,
                       hline.after = tx$hline_after)
    
    do.call("print.xtable", tx_towrite)

    landscape <- F

    if ("landscape" %in% names(tx)) {
        if (tx$landscape) {
            landscape <- T
        }
    }

    ## landscape <- fifelse("landscape" %in% names(tx),
    ##                      fifelse(tx$landscape == T, T, F),
    ##                      ## tx$landscape,
    ##                      F)

    wtbl_pdf(tblname, landscape)

    return(invisible(NULL))

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
        texput_ending <- paste0("\\end{landscape}", texput_ending, collapse = "\n")

    } else {
        texput_file <- "texput.tex"
    }


    texputted_filename <- paste0("/tmp/", tblname, "_texput.tex")

    ## for now, copy texput files from dropbox. FIXME: put texput files into package dir
    cmd_copy_texput <- sprintf("cp /home/johannes/Dropbox/technical_stuff_general/dotfiles/%s %s",
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
fmt_cell <- function(coef, se, pvalue, wcptbl, type) {

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
    if (wcptbl) cell_proc <- sprintf("$%s$", cell_proc)

    return(cell_proc)

}


#' generate the multicolumn latex commands for variable groups
#' uses the grouping label (rather than "raw" grouping id) to save a merge step
#' to use in add_to_row
#' @param dtx  the (ordered) data.frame, needs to have the grouping variable
#' @param grp the variable to group by, should already be the grp_label
#' @export
#' @return data.table with columns grpstr (the group latex string),  pos (its position) grp_intern (grp vrbl)
gc_grpstrs <- function(dtx, grp, nbr_cols) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
   
    copy(dtx)[, nbr := 1:.N] %>%
        .[, grp_intern := get(grp)] %>% # set up separate variable to still have NSE even when variable changes
        .[, .(pos = min(nbr)-1), grp_intern] %>%
        .[, grpstr := sprintf("\\multicolumn{%s}{l}{\\textbf{%s}} \\\\ \n", nbr_cols, grp_intern)]
        
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

    sig_note <- sprintf("\\hline \n \\multicolumn{%s}{l}{\\footnotesize{%s}}\n", ncol, sig_note_vlus)

    return(sig_note)    

}
