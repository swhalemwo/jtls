


## overall FIXME
## - replace dt_grid with something more general (not all (training) data is grid)
## - generalize for more than stringdist values


#' separate stringdist features into those based on q-grams and those who aren't
#'
#' @param l_feats list of features
gl_modnoq_qmod <- function(l_feats) {
    ## construct feature vectors of which features to generate:
    ## for no-qgram a vector, for qgram methods dt_qmod (with name and q spec)
    

    ## sort into q and noq
    l_mod_qindex <- grepl("\\d", l_feats)
    l_mod_noq <- l_feats[!l_mod_qindex] %>% gsub("strdist_", "", .)

    dt_qmod <- data.table(mod = sub("strdist_([a-z_]+)_\\d+$", "\\1", l_feats[l_mod_qindex]),
                          q = sub(".*_(\\d+)$", "\\1", l_feats[l_mod_qindex]))
    
    return(
        list(l_mod_noq = l_mod_noq,
             dt_qmod = dt_qmod))
}




#' train and write a smaller XGB model (most influential features) based of full model
#'
#' @param r_xgb full model
#' @param dt_grid_blank data.table with strings to match
#' @param c_params params to pass to XGB, also other stuff (names, n_rounds)
#' @export
gr_xgb_smol <- function(r_xgb, dt_grid_blank, c_params) {
    ## browser()
    ## FIXME: data dependencies still rely on globals    

    dt_topfeat <- gd_xgb_topfeat(r_xgb)

    ## get only most relevant features,
    l_modnoq_qmod <- gl_modnoq_qmod(dt_topfeat[, Feature])
    l_mod_noq <- l_modnoq_qmod %>% chuck("l_mod_noq")
    dt_qmod <- l_modnoq_qmod %>% chuck("dt_qmod")
    
    ## only construct most relevant features
    dt_grid_wfeat <- gd_grid_wfeat(dt_grid_blank, name1 = chuck(c_params, "name1"),
                                   name2 = chuck(c_params, "name2"),
                                   dt_qmod = dt_qmod, l_mod_noq = l_mod_noq)

    ## FIXME: so far only strdist
    l_cols_feat_smol <- keep(names(dt_grid_wfeat), ~.x %in% dt_topfeat[, Feature])

    l_mat_train_test <- gl_mat_train_test(dt_grid_wfeat, l_cols_feat = l_cols_feat_smol, frac_test = 0.8)
    mat_train_smol <- chuck(l_mat_train_test, "mat_train")
    mat_test_smol <- chuck(l_mat_train_test, "mat_test")    


    ## mat_train <- xgb.DMatrix(dt_grid_train[, .SD, .SDcols = l_cols_feat] %>% as.matrix,
    ##                      label = dt_grid_train$match)

    l_watch_smol <- list(train = mat_train_smol, test = mat_test_smol)

    r_xgb_smol <- xgb.train(params = c_params %>% chuck("c_params_xgb"), data = mat_train_smol,
                            nrounds =c_params %>% chuck("n_rounds"),
                            verbose  = 1, watchlist = l_watch_smol)

    dt_assess <- gd_xgb_assess(r_xgb_smol, mat_test = mat_test_smol)
    print(dt_assess)
    return(r_xgb_smol)
}


#' generate data.table of top features from an XGB model
#'
#' @param r_xgb XGB model
#' @param thld threshold of cumulative gain
gd_xgb_topfeat <- function(r_xgb, thld = 0.8) {
    ## use smaller, faster model: only take most influential predictors
    ## feature construction is expensive so can use that to filter down number of cases to check

    dt_topfeat <- xgb.importance(model = r_xgb) %>% .[, cum_gain := cumsum(Gain)] %>% .[cum_gain < thld]

    return(dt_topfeat)
}



#' generate strdist features involving q-grams
#'
#' @param dtx data.table with name1, name2 columns
#' @param dt_qmod data.table specifying q-gram strdists: name and q spec
gd_strfeat_wq <- function(dtx, dt_qmod) {
    ## generate string features for string similarities with q

    dt_strfeat_wq <- dtx %>% copy %>%
        .[, paste0(dt_qmod[, sprintf("strdist_%s_%s", mod, q)]) :=
                (map2(dt_qmod[, mod], dt_qmod[, q],
                      ~stringdist(tolower(name1), tolower(name2), method = .x, q = .y))), name1]

    return(dt_strfeat_wq)
}

#' generate strdist features based on just name
#'
#' @param dtx data.table with name1, name2
#' @param l_mod_noq vector arguments to stridst
gd_strfeat_noq <- function(dtx, l_mod_noq) {
    ## generate string similarity features for which there is no q (only single input)
    dt_strfeat_noq <- dtx %>% copy %>%
        .[, paste0("strdist_", l_mod_noq) :=
                map(l_mod_noq, ~stringdist(tolower(name1),
                                           tolower(name2), method = .x)), name1]
        

    return(dt_strfeat_noq)
}
    

#' generate data.table with stringdist features from name columns
#'
#' @param dt_grid_table data.table with name1, name2 to generate strdist columns
#' @param name1 string of name1
#' @param name2 string of name2
#' @param dt_qmod data.table specifying q-gram strdists: name and q spec
#' @param l_mod_noq vector of string dists not using qgram
gd_grid_wfeat <- function(dt_grid_blank, name1, name2, dt_qmod = NULL, l_mod_noq = NULL) {
    ## construct features for similarity matching
    
    dt_grid_blank %>% setnames(old = c(name1, name2), new = c("name1", "name2"))
    

    if (is.null(dt_qmod)) {
        l_qs <- 1:5 # qgrams for qmethods
        l_qmods <- c("qgram", "jaccard", "cosine") # methods using Q

        dt_qmod <- CJ(q = l_qs, mod = l_qmods)## [, sprintf("%s_%s", mod, q)]
    }

    if (is.null(l_mod_noq)) {
        ## "hamming" creates infinite dist -> yeet
        l_mod_noq <- c("osa", "lv", "dl", "lcs", "jw")
    }

    
    dt_grid_wfeat_q <- gd_strfeat_wq(dt_grid_blank, dt_qmod)

    dt_grid_wfeat_both <- gd_strfeat_noq(dt_grid_wfeat_q, l_mod_noq)
    
    ## dt_grid_blank <- dt_grid_blank[1:1e4]

    ## generate string similarities for q mods
    
    dt_grid_wfeat_both %>% setnames(old = c("name1", "name2"), new = c(name1, name2))

    ## fill up missing columns
    l_cols_feat <- paste0("strdist_", c(dt_qmod[, sprintf("%s_%s", mod, q)], l_mod_noq))
    setnafill(dt_grid_wfeat_both, fill = NA, nan = NA, cols = l_cols_feat)

    dt_grid_wfeat_both <- dt_grid_wfeat_both[sample(1:.N, size = .N)] # shuffle

    return(dt_grid_wfeat_both)

}



#' assess performance of XGB model: accuracy, precision, recall, F1
#'
#' @param r_xgb XGB model
#' @param mat_test DMatrix with same columns that r_xgb was trained on
#' @param thld threshold for classification
#' @param return_data whether to return data
gd_xgb_assess <- function(r_xgb, mat_test, thld = 0.5, return_data = F) {
    
    
    ## dt_grid_test[, match_pred_num := predict(r_xgb, mat_test)][, match_pred := 1*(match_pred_num > thld)]

    dt_porf <- data.table(match = getinfo(mat_test, "label"), match_pred = 1*(predict(r_xgb, mat_test)> thld))

    ## mat_test %>% str
    
    mat_confuse <- dt_porf[, table(Pred = match_pred, actual = match)]

    accuracy <- sum(diag(mat_confuse)) / sum(mat_confuse)
    precision <- mat_confuse[2, 2] / sum(mat_confuse[2, ])
    recall <- mat_confuse[2, 2] / sum(mat_confuse[, 2])
    f1_score <- 2 * ((precision * recall) / (precision + recall))

    dt_assess <- data.table(
        accuracy = accuracy,
        precision = precision,
        recall = recall,
        f1_score = f1_score)

    
    if (return_data) {
        ret_obj <- list(dt_grid_test = dt_grid_test, dt_assess = dt_assess)
    } else {
        ret_obj <- dt_assess
    }
    

    return(ret_obj)
}

#' generate and filter dt_grid_blank based on predicted score of r_xgb_smol
#'
#' @param dt_grid_blank data.table with name1, name2 cols
#' @param r_xgb_smol XGB model
#' @param name1 name1 column
#' @param name2 name2 column
#' @param thld threshold for filtering
gd_dt_smol <- function(dt_grid_blank, r_xgb_smol, name1, name2, thld = 0.0001) {
    ## idea: apply a smaller model first (less time spent constructing features, which is expensive)

    ## first see which columns are needed
    # l_topfeat <- gd_xgb_topfeat(r_xgb)[, Feature]
    # l_topfeat <- xgb.importance(model = r_xgb_smol)[, Feature]
    l_topfeat <- r_xgb_smol$feature_names

    ## FIXME: replace with gl_modnoq_qmod

    ## sort into q and noq
    l_mod_qindex <- grepl("\\d", l_topfeat)
    l_mod_noq <- l_topfeat[!l_mod_qindex] %>% gsub("strdist_", "", .)

    dt_qmod <- data.table(mod = sub("strdist_([a-z_]+)_\\d+$", "\\1", l_topfeat[l_mod_qindex]),
                          q = sub(".*_(\\d+)$", "\\1", l_topfeat[l_mod_qindex]))

    ## construct feature dt
    dt_feat_smol <- gd_grid_wfeat(copy(dt_grid_blank), name1, name2, dt_qmod = dt_qmod, l_mod_noq = l_mod_noq)

    mat_pred_smol <- xgb.DMatrix(dt_feat_smol[, .SD, .SDcols = l_topfeat] %>% as.matrix)

    dt_feat_smol[, match_pred := predict(r_xgb_smol, mat_pred_smol)]

    ## gd_xgb_assess(r_xgb_smol, dt_feat_smol, mat_pred_smol, thld = 0.0001) # gives recall of 0.994, not ideal but hopefully good enough
    ## and results in 10x data decrease
    
    dt_feat_smol[match_pred >= thld]
    

    ## then construct those features for dt_grid_blank
    
}

#' XGB hyperparameter tuning workhorse function
#'
#' @param mat_train training data
#' @param mat_test test data
#' @param eta learning rate
#' @param max_depth max depth
#' @param n_rounds n_rounds
#' @param subsample subsample
assess_xgb_params <- function(mat_train, mat_test, eta, max_depth, n_rounds, subsample) {
    ## trains model with certain parameters and evaluates model performance
    

    c_params <- list(
        objective = "binary:logistic",
        eval_metric = "logloss",
        eta = eta,
        max_depth = max_depth,
        subsample = subsample)

    r_xgb <- xgb.train(params = c_params, data = mat_train, nrounds =n_rounds, verbose  = 1, watchlist = l_watch)
    gd_xgb_assess(r_xgb, mat_test)
}


#' XGB hyperparameter testing main function
#' 
gd_hyperparam_tuning <- function() {

    ## FIXME: assumes mat_train/test are global
    ## somewhat systematic parameter exploration
    ## set up params
    dt_boost_paramcbns <- expand.grid(
        eta = c(0.01, 0.05, 0.2, 0.5),
        max_depth = c(2,4,6,8),
        n_rounds = c(20,50,100),
        subsample = c(0.5,1)) %>% adt

    l_boost_paramcbns <- dt_boost_paramcbns %>% split(1:nrow(.))


    l_res <- map(l_boost_paramcbns, ~do.call(assess_xgb_params,
                                             c(list(mat_train = mat_train, mat_test = mat_test), .x)))

    dt_paramres <- l_res %>% rbindlist %>%
        cbind(dt_boost_paramcbns)

    return(dt_paramres)
}


#' generate training/testing data for XGB models
#'
#' @param dt_grid_wfeat data.table with features
#' @param l_cols_feat vector of columns in dt_grid_wfeat to use as features
#' @param frac_test fraction to use as training data
#' @export
gl_mat_train_test <- function(dt_grid_wfeat, l_cols_feat, frac_test = 0.8) {
        
    ## train_index <- createDataPartition(dt_grid_wfeat$match, p = frac_test, list = FALSE)
    train_index <- sample(1:dt_grid_wfeat[, .N], size =  dt_grid_wfeat[, .N]*frac_test,
                          prob = rep(frac_test, dt_grid_wfeat[, .N]))
    ## sample 

    dt_grid_train <- dt_grid_wfeat[train_index, ]
    dt_grid_test <- dt_grid_wfeat[-train_index, ]

    
    mat_train <- xgb.DMatrix(dt_grid_train[, .SD, .SDcols = l_cols_feat] %>% as.matrix,
                             label = dt_grid_train$match)

    mat_test <- xgb.DMatrix(dt_grid_test[, .SD, .SDcols = l_cols_feat] %>% as.matrix,
                            label = dt_grid_test$match)

    return(list(mat_train = mat_train,
                mat_test = mat_test))
    

}


#' generate XGB model of matching names based on string distance
#'
#' @param dt_grid_blank data.table with columns specified as name1/2 in c_params
#' @param c_params nested list: general XGB params, also params arg for xgb.train
#' @export
gr_xgb <- function(dt_grid_blank, c_params) {
    
    
    ## can't assume binary classification
    dt_grid_wfeat <- gd_grid_wfeat(dt_grid_blank,
                                   chuck(c_params, "name1"),
                                   chuck(c_params, "name2"))

    ## FIXME: so far only strdist
    l_cols_feat <- keep(names(dt_grid_wfeat), ~grepl("^strdist", .x))

    l_mat_train_test <- gl_mat_train_test(dt_grid_wfeat, l_cols_feat = l_cols_feat, frac_test = 0.8)
    mat_train <- chuck(l_mat_train_test, "mat_train")
    mat_test <- chuck(l_mat_train_test, "mat_test")    

    ## setup training/test data
    

    l_watch <- list(train = mat_train, test = mat_test)

    r_xgb <- xgb.train(params = c_params %>% chuck("c_params_xgb"),
                       data = mat_train,
                       nrounds =c_params %>% chuck("n_rounds"),
                       verbose  = 1, watchlist = l_watch)

    gd_xgb_assess(r_xgb,  mat_test) %>% print

    return(r_xgb)
        

}



## ** main

## hyperparameter tuning exploration
## FIXME should be more systematic
## dt_paramres_long <- dt_paramres %>% melt(id.vars = names(dt_boost_paramcbns))
    

## fx <- sprintf("value ~ mvsw(%s)", paste0(names(dt_boost_paramcbns), collapse = ",")) %>% as.formula
## feols(fx, dt_paramres_long[variable == "recall"])

## dt_paramres_long[variable == "recall"] %>% 
##     ggplot(aes(x = factor(n_rounds), y = value, color = factor(eta))) + geom_point() +
##     facet_grid(max_depth~subsample)

## dt_paramres[order(-recall)]

## ## dt_paramres %>% ggplot(aes(x = n_rounds, 
