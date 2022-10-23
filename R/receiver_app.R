

css_receive <- '

html, body {
    height: 100%;
    margin: 0;
    padding: 0;
    overflow: hidden;
}

#test_demo {
    overflow: hidden;
    border: solid 1px black;
    height: 800px;
    width: 100%;
    min-height: 800px;
}




'

test_launcher_receive <- function(launcher_url = "https://adaptiveeartraining.com/test-launcher/") {


  shiny::shinyApp(
    ui = shiny::fillPage(
      shiny::tags$head(shiny::tags$style(css_receive)),
      shiny::uiOutput("test_demo")),

    server = function(input, output, session) {

      output$test_demo <- shiny::renderUI({

        query <- shiny::parseQueryString(session$clientData$url_search)

        if(length(query) == 0) {

          tags$p("You must launch this app via the URL: ",
                 tags$a(href = launcher_url, launcher_url))

        } else if(length(query) > 0 & is.null(query$test_fun_name)) {
          tags$p("No test function name. You must launch this app via the URL: ",
                 tags$a(href = launcher_url, launcher_url))
        } else {

          test_fun_name <- query$test_fun_name

          test_fun_all <- get_test_fun(test_fun_name)
          test_fun <- test_fun_all$test_fun
          types <- test_fun_all$types
          defaults <- test_fun_all$defaults


          url_params <- names(query)
          url_params <- url_params[!url_params == "test_fun_name"]


          arg_list <- purrr::map(url_params, function(par) {

            par_val <- default_if_no_parameter(par, query, defaults)

            par_val <-  sort_type(types, par, par_val)

            par_val
          })

          names(arg_list) <- url_params
          #browser()
          arg_list <- sort_arg_names_and_vals(arg_list, names(arg_list), test_fun_name)

          item_bank <- get_item_bank(arg_list$item_bank)

          arg_list$app_name <- "demo"

          arg_list$item_bank <- item_bank

          do.call(test_fun, args = arg_list)

        }


      })
    }
  )
}


sort_type <- function(types, par, par_val) {

  if(types[[par]] == "logical") {
    par_val <- as.logical(par_val)
  } else  if(types[[par]] == "character") {
    par_val <- as.character(par_val)
  } else  if(types[[par]] == "numeric") {
    par_val <- as.numeric(par_val)
  } else {
    warning("Unknown type.")
  }
  par_val
}

get_correct_types <- function(test_fun_name) {
  if(test_fun_name == "SAA_standalone") {
    types <- SAA_types
  } else if(test_fun_name == "PBET_standalone") {
    types <- PBET_types
  } else if(test_fun_name == "SRT_standalone") {
    types <- SRT_types
  } else if(test_fun_name == "SST_standalone") {
    types <- SST_types
  } else if(test_fun_name == "PDT_standalone") {
    types <- PDT_types
  } else {
    stop("Test function not known")
  }
  types
}




# 127.0.0.1:4737/?num_items_long_tones=1&num_items_rhythmic=1&num_items_arrhythmic=1
# 127.0.0.1:4737/?headphones_test=TRUE


# receiver_app()
