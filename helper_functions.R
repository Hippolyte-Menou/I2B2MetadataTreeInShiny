library(data.tree)
library(shiny)
library(shinyTree)
library(shinyjs)


fixIconName <- function(icon){
  ## - 'yes' branch of 'if' covers everything which should not be changed
  ##   e.g. "/images/ball.jpg" or "fa fa-file
  ## - 'no' branch of 'if' covers all cases which need to be changed:
  ##   use regex (str_match) to capture groups: 
  ##     * group 1 is either 'glyphicon', 'fa' or 'NA' (if not present)
  ##     * group 2 is the rest wihtout a potential dash '-'
  ##     * if group 1 is empty set it to 'fa'
  ##     * paste the pieces together
  res <- ifelse(grepl("[/\\]|(glyphicon|fa) \\1-", icon), 
                icon, 
                {
                  parts <- str_match(icon, "(glyphicon|fa)*-*(\\S+)")
                  parts[, 2] <- ifelse(is.na(parts[, 2]), "fa", parts[, 2])
                  paste(parts[, 2], paste(parts[, 2], parts[, 3], sep = "-")) 
                })
  ## if NULL was given as parameter res will be length zero
  if (!length(res)) {
    NULL
  } else {
    res
  }
}

treeToJSON <- function(tree, 
                       keepRoot = FALSE,
                       topLevelSlots = c("default", "all"),
                       createNewId = TRUE,
                       pretty = FALSE) {
  ## match against "default"/"all", if this returns an error we take topLevelSlots as is
  ## i.e. a vector of names to keep
  if (!requireNamespace("data.tree", quietly = TRUE)) {
    msg <- paste("library", sQuote("data.tree"), "cannot be loaded. Try to run",
                 sQuote("install.packages(\"data.tree\")"))
    stop(msg, domain = NA)
  }
  nodesToKeep <- list(default = c("id", "text", "icon", "state",
                                  "li_attr", "a_attr", "type"),
                      all     = NULL)
  topLevelSlots <- tryCatch(nodesToKeep[[match.arg(topLevelSlots)]],
                            error = function(e) topLevelSlots)
  node_to_list <- function(node, 
                           node_name = NULL) {
    fields <- mget(node$fields, node)
    NOK <- sapply(fields, function(slot) !is.atomic(slot) && !is.list(slot))
    if (any(NOK)) {
      msg <- sprintf(ngettext(length(which(NOK)),
                              "unsupported slot of type %s at position %s",
                              "unsupported slots of types %s at positions %s"),
                     paste0(dQuote(sapply(fields[NOK], typeof)),
                            collapse = ", "),
                     paste0(sQuote(names(fields)[NOK]),
                            collapse = ", "))
      warning(msg,
              domain = NA)
      fields[NOK] <- NULL
    }
    if (is.null(fields$text)) {
      fields$text <- if(!is.null(fields$name)) fields$name else node_name
    }
    fields$icon <- fixIconName(fields$icon)
    if (!is.null(fields$state)) {
      valid_states <- c("opened", "disabled", "selected", "loaded")
      states_template <- stats::setNames(rep(list(FALSE), length(valid_states)),
                                         valid_states)
      NOK <- !names(fields$state) %in% valid_states
      if (any(NOK)) {
        msg <- sprintf(ngettext(length(which(NOK)),
                                "invalid state %s",
                                "invalid states %s"),
                       paste0(dQuote(names(fields$state)[NOK]),
                              collapse = ", "))
        warning(msg,
                domain = NA)
      }
      states_template[names(fields$state[!NOK])] <- fields$state[!NOK]
      fields$state <- states_template
    }
    if (is.null(topLevelSlots)) {
      slots_to_move <- character(0)
    } else {
      slots_to_move <- names(fields)[!names(fields) %in% topLevelSlots]
    }
    data_slot <- fields[slots_to_move]
    if (length(data_slot)) {
      fields$data <- data_slot
      fields[slots_to_move] <- NULL
    }
    if (!is.null(node$children)) {
      ## purrr::imap would make code cleaner but did not want to add another dependency
      ## unname needed to create an JSON array as opposed to an JSON object
      fields$children <- unname(lapply(names(node$children), 
                                       function(i) node_to_list(node$children[[i]],
                                                                i)))
    }
    fields
  }
  ## clone tree as we do not want to alter the original tree
  tree <- data.tree::Clone(tree)
  nodes <- data.tree::Traverse(tree, filterFun = data.tree::isNotRoot)
  old_ids <- data.tree::Get(nodes, "id")
  if (createNewId) {
    if (any(!is.na(old_ids))) {
      warning(paste("slot",
                    dQuote("id"), 
                    "will be stored in",
                    dQuote("id.orig")),
              domain = NA)
      data.tree::Set(nodes, id.orig = old_ids)
    }
    new_ids <- seq_along(nodes)
  } else {
    if (any(is.na(old_ids)) ||
        any(duplicated(old_ids))) {
      warning(paste("old ids are invalid (duplicated values or NA),",
                    "creating new ids"),
              domain = NA)
      new_ids <- seq_along(nodes)
    } else {
      new_ids <- old_ids
    }
  }
  
  data.tree::Set(nodes, id = new_ids)
  treeList <- node_to_list(tree)
  if (!keepRoot) {
    ## to prune off the root node return the first children list
    treeList <- treeList$children
  }
  ## use as.character b/c updateTree needs an unparsed JSON string, as 
  ## the parsing is done in shinyTree.js
  as.character(jsonlite::toJSON(treeList, 
                                auto_unbox = TRUE, 
                                pretty = pretty))
}

