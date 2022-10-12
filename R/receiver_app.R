

receiver_app <- function() {


  shiny::shinyApp(
    ui = shiny::uiOutput("test_demo"),

    server = function(input, output, session) {

      output$test_demo <- shiny::renderUI({

        query <- shiny::parseQueryString(session$clientData$url_search)

        types <- get_correct_types()


        url_params <- names(query)
        arg_list <- purrr::map(url_params, function(par) {

          par_val <- default_if_no_parameter(par, query, defaults) %>% sort_type()

          par_val
        })

        names(arg_list) <- url_params

        arg_list$app_name <- "demo"

        test_name <- arg_list$test_name
        test_fun <- get_test_fun(test_name)
        arg_list$test_name <- NULL


        do.call(test_fun, args = arg_list)
      })
    }
  )
}


sort_type <- function(types) {
  if(types[[par]] == "logical") {
    par_val <- as.logical(par_val)
  }
}

get_correct_types <- function(test_fun_name) {
  if(test_fun_name == "SAA_standalone") {
    types <- SAA_input_types
  } else if(test_fun_name == "PBET_standalone") {
    types <- PBET_input_types
  } else if(test_fun_name == "SRT_standalone") {
    types <- SRT_input_types
  } else if(test_fun_name == "SST_standalone") {
    types <- SST_input_types
  } else if(test_fun_name == "PDT_standalone") {
    types <- PDT_input_types
  } else {
    stop("Test function not known")
  }
  types
}


# 127.0.0.1:4737/?num_items_long_tones=1&num_items_rhythmic=1&num_items_arrhythmic=1
# 127.0.0.1:4737/?headphones_test=TRUE


# receiver_app()
