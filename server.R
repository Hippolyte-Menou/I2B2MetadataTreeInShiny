library(tidyverse)
library(data.tree)

library(shiny)
library(shinyjs)
library(shinyTree)

library(rclipboard)

source("helper_functions.R")


data <- read_csv("metadata.csv")

dt <- data %>%
  as_tibble() %>% 
  mutate(C_FULLNAME = paste0("\\i2b2\\", C_FULLNAME),
         C_HLEVEL = if_else(C_VISUALATTRIBUTES == "RA", str_count(C_FULLNAME, "\\\\") - 2, C_HLEVEL),  # C_HLEVEL of modifiers are all 1, changing it to the correct number (either 2 or 3)
         C_FULLNAME2 = str_replace_all(C_FULLNAME, regex("\\\\( ){1,3}"), "\\\\"),  # remove leading whitespace on nodes in the full path
         C_FULLNAME2 = str_replace_all(C_FULLNAME2, regex("( ){1,11}\\\\"), "\\\\"),  # remove trailing whitespace on nodes in the full path
         sticon = recode(C_VISUALATTRIBUTES, "FA"="glyphicon glyphicon-folder-close", "LA"="glyphicon glyphicon-file", "LI"="glyphicon glyphicon-file red", "RA"="glyphicon glyphicon-file blue"), 
         type = C_VISUALATTRIBUTES)



df_to_tree <- function(df){
  # returns a data.tree
  df %>% as.Node(pathName = "C_FULLNAME2", pathDelimiter = "\\") 
}

df_to_treelist <- function(df){
  # returns a nested list with attributes
  # structure("a" = list("aa" = list(), "ab" = list(), "ac" = list()), attr1 = F, attr2 = "rr")
  f <- function(l){
    s <- structure(list())
    attributes(s) <- split(unname(l), names(l))
    s
  }
  a <- apply(df, 1, f)
  names(a) <- lapply(strsplit(df$C_FULLNAME2, "\\", T), function(s){tail(s, n=1)})
  a
}

get_path <- function(node, ancestors){
  paste("\\i2b2", paste(c(ancestors, node), collapse = "\\"), "", sep = "\\")
}

get_line <- function(df, node_path){
  df %>% filter(C_FULLNAME2 == node_path)
}

request_children <- function(node_path, node_level){
  # request children with a fullname that contains the path of the current node and with a level 1 above
  dt %>% filter(C_HLEVEL == node_level + 1, str_detect(C_FULLNAME2, fixed(node_path)))
}

search_in_df <- function(search_term, search_on="C_NAME", use_regex = F){
  if (use_regex){
    dt %>% filter(str_detect(!!as.symbol(search_on), regex(search_term, ignore_case=T)))
  }else{
    dt %>% filter(str_detect(!!as.symbol(search_on), fixed(search_term, ignore_case=T)))
  }
}

explore_tree_df <- dt %>% filter(C_HLEVEL == 1) # dataframe of the starting tree in the explore tab
explore_tree <- df_to_treelist(explore_tree_df) # nested list with attributes of the starting tree in the explore tab

search_tree_df <- search_in_df("hyp(o|er)gly", "C_NAME", T) # dataframe   "----------------" in the search tab
search_tree <- df_to_tree(search_tree_df)                   # nested list "----------------" in the search tab


shinyServer(function(input, output, session) {
  
  v <- reactiveValues()
  v$df <- explore_tree_df
  v$requests <- c() # store previous requests for children to prevent doing them twice
  
  v$selected <- NULL
  
  v$df2 <- search_tree_df
  
  output$explore_tree <- renderTree({
    explore_tree 
  })
  
  output$selected <- reactive({
    # Controls conditional panel
    length(v$selected) > 0
  })
  
  observeEvent(input$explore_tree,  {
    v$selected <- get_selected(input$explore_tree)
    if (length(v$selected) > 0){
      v$node_name <- v$selected[[1]][1]
      v$node_ancestors <- attr(v$selected[[1]], 'ancestry')
      v$node_path <- get_path(v$node_name, v$node_ancestors)
      v$node_line <- get_line(v$df, v$node_path)
      v$fullname <- v$node_line$C_FULLNAME
      v$node_level <- v$node_line$C_HLEVEL
      v$is_leaf <- v$node_line$C_VISUALATTRIBUTES %in% c("LA", "RA", "LI")
      v$code <- v$node_line$C_BASECODE
    }
  })
  
  output$code <- renderText({
    v$code
  })
  
  output$node_name <- renderText({
    v$node_name
  })
  
  output$fullname <- renderText({
    v$fullname
  })
  
  output$clip <- renderUI({
    rclipButton("clipbtn", "", str_replace_all(v$fullname, fixed("\\"), fixed("\\\\")), icon("clipboard")) # with the button, escape chars are intrepeted so \\ have to be doubled
  })
  
  output$codes <- renderTable({
    # render a table with all node whose C_BASECODE match the one of the currently selected node
    if (length(v$selected)>0) {
      if (v$code != "PROT:N/A"){ # This code returns 33k matchs and is meaningless
        dt %>% 
          filter(C_BASECODE == v$code) %>% 
          select(C_NAME) %>% 
          arrange(nchar(C_NAME)) 
      }
    }
  })
  
  
  #Append children
  observeEvent(v$selected, {
    #print(v$selected)
    if (all(
      #input$fetch_children,          # controls if children should be appended on selection
      length(v$selected) > 0,         # a node was selected
      nrow(v$node_line) == 1,         # node was found in the dataframe
      v$is_leaf == F,                 # node is not a leaf
      !(v$node_path %in% v$requests)  # request was not already done
    )){
      v$requests <- c(v$requests, v$node_path) # store the request
      children <- request_children(v$node_path, v$node_level)
      if (nrow(children) > 0) { # children found
        v$df <- v$df %>% bind_rows(children) #%>% distinct()
        
        path_list <- c(v$node_ancestors, v$node_name)
        
        t <- input$explore_tree 
        
        # insert the structure(list(), attr = ,) on the corresponding node
        pluck(t, !!!path_list) <- structure(df_to_treelist(children), stselected = T, stopened = T, sticon="glyphicon glyphicon-folder-open")  # stopened = input$open_children
        
        updateTree(session, "explore_tree", t)}
    }
    
  })
  
  matching_codes <- reactive({
    if (!is.null(v$code)){
      if (v$code != "PROT:N/A"){
        dt %>% 
          filter(C_BASECODE == v$code)
      }
    }
  })
  
  matching_code_json <- reactive({
    matching_codes() %>% 
      df_to_tree() %>% 
      treeToJSON()
  })
  
  output$matching_code_tree <- renderTree({
    if (!is.null(v$code)){
      matching_code_json()
    }
  })
  
  observeEvent(matching_codes(), {
    updateTree(session, "matching_code_tree", matching_code_json())
  })
  
  observeEvent(input$open_matching_code, {
    runjs(HTML('$("#matching_code_tree").jstree("open_all");'))
  })
  
  
  #############
  # Search
  #############
  
  output$search_tree <- renderTree({
    treeToJSON(search_tree) # use treeToJSON because there are multiple levels
  })
  
  v$trigger_search <- 0 # to merge the update from the action button and the dynamic search
  
  observeEvent(input$search_tree, {
    v$selected_search_tree <- get_selected(input$search_tree)
    
    if (length(v$selected_search_tree) > 0){
      v$s_node_name <- v$selected_search_tree[[1]][1]
      v$s_node_ancestors <- attr(v$selected_search_tree[[1]], 'ancestry')
      v$s_node_path <- get_path(v$s_node_name, v$s_node_ancestors)
      v$s_node_line <- get_line(dt, v$s_node_path)
      v$s_fullname <- v$s_node_line$C_FULLNAME
      
      v$s_node_level <- v$s_node_line$C_HLEVEL
      v$s_is_leaf <- v$s_node_line$C_VISUALATTRIBUTES %in% c("LA", "RA", "LI")
      v$s_code <- v$s_node_line$C_BASECODE
      
    }
  })
  
  output$s_selected <- reactive({
    # command the conditional panel
    length(v$selected_search_tree) > 0
  })
  
  output$s_code <- renderText({
    v$s_code
  })
  
  output$s_node_name <- renderText({
    v$s_node_name
  })
  
  output$s_fullname <- renderText({
    v$s_fullname
  })
  
  output$s_clip <- renderUI({
    rclipButton("clipbtn", "", str_replace_all(v$s_fullname, fixed("\\"), fixed("\\\\")), icon("clipboard"))
  })
  
  observeEvent(input$perform_search, {
    v$trigger_search <- v$trigger_search + 1
  })
  
  observeEvent(input$text_to_search, {
    if(input$dynamic_search){
      v$trigger_search <- v$trigger_search + 1
    }
  })
  
  observeEvent(v$trigger_search, {
    search_results <- search_in_df(input$text_to_search, input$search_on, input$use_regex)
    if (nrow(search_results) > 0){
      v$df2 <- search_results
    }
  })

  observeEvent(v$df2, {
    updateTree(session, "search_tree", treeToJSON(df_to_tree(v$df2)))
  })
  
  observeEvent(input$open_all, {
    runjs(HTML('$("#search_tree").jstree("open_all");'))
  })
  
  outputOptions(output, "selected", suspendWhenHidden = F)
  outputOptions(output, "s_selected", suspendWhenHidden = F)
  
})
