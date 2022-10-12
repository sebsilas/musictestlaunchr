
# app_launcher()

app_launcher <- function(root_url = "http://www.adaptiveeartraining.com/test-demo/") {

  ui <- shiny::fluidPage(
    shiny::selectInput("test", label = "Test", choices = musicassessr::list_official_tests()),
    shiny::uiOutput("reactive_ui")
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    output$reactive_ui <- shiny::renderUI({

      test_fun_name <- input$test %>%
        strsplit(., split = "::") %>%
        purrr::pluck(1) %>%
        purrr::pluck(2)


      test_fun <- get_test_fun(test_fun_name)

      remaining_args <- remaining_args(test_fun$test_fun,  test_fun$args_to_remove)

      names <- remaining_args$names
      defaults <- remaining_args$defaults

      objs <- compile_shiny_objects(names, test_fun$types, defaults)

      shiny::observeEvent(input$get_launch_code, {
        #browser()
        code_to_write <- produce_args(test_fun_name, names, defaults) # shouldn't just be defaults!
        output$app_code <- shiny::renderText(code_to_write)
      })

      shiny::observeEvent(input$launch_app, {

        # YO!
        pars <- defaults
        url <- prepare_url_parameters(pars, root_url)

        shinyjs::runjs(paste0('window.open(\"',url,'\", "_blank").focus();'))
      })

      shiny::tags$div(

        shinyjs::useShinyjs(),

        shiny::tags$head(shiny::tags$style(css)),


        shiny::fluidRow(
          shiny::column(4,

                        shiny::titlePanel("App Launcher"),

                        shiny::selectInput("item_bank", label = "Item Bank", choices = itembankr::list_official_item_banks()),


                        objs$left),
          shiny::column(4, objs$right),

          shiny::column(4,
                        shiny::actionButton("get_launch_code", "Get Launch Code"),

                        shiny::actionButton("launch_app", "Launch App"),

                        shiny::tags$div(shiny::textOutput("app_code"))
          )
        ),

        shiny::tags$script('document.getElementById("get_launch_code").addEventListener("click", function() {
                       document.getElementById("app_code").style.visibility = "visible";});
                       ')
      )


    })



  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}




get_test_fun_name <- function(test_fun) {
  test_fun_name <- as.character(substitute(test_fun))
  test_fun_name <- paste0(test_fun_name[2], test_fun_name[1], test_fun_name[3])
}


css <- "#app_code { font-size:12px; font-style:italic;overflow-y:scroll; width: 400px;
                                      background: ghostwhite; border: solid 1px #f1e9f5; border-radius: 3px;
                                      margin: 10px; padding: 10px; visibility: hidden;}"




produce_args <- function(test_fun_name, arg_names, arg_names_and_vals) {

  arg_list <- purrr::map2_chr(arg_names, arg_names_and_vals, function(arg_name, val) {
    paste0(arg_name, ' = ', deparse(val))
  })

  arg_list <- arg_list %>% paste0(collapse = ", ")

  full_fun_as_text <- paste0(test_fun_name, '(', arg_list, ')')
}




remaining_args <- function(test_fun, args_to_remove) {

  arg_names_and_vals <- formals(test_fun)

  if(!identical(test_fun, PDT::PDT_standalone)) { # the num_items argument for PDT doesn't need unnesting
    arg_names_and_vals <- arg_names_and_vals %>%
      nested_list_to_renamed_list(item_to_unnest = "num_items")
  }

  # remove "..."

  arg_names_and_vals$... <- NULL

  arg_names <- names(arg_names_and_vals)

  if(!is.null(args_to_remove)) {
    arg_names_and_vals <- arg_names_and_vals[!arg_names %in% args_to_remove]
    arg_names <- names(arg_names_and_vals)
  }



  list(names = arg_names, defaults = arg_names_and_vals)
}


# create_ui <- function(test_fun, arg_names, types = NULL) {
#
#   cat('Argument list should be: ', arg_names)
#
#   if(!setequal(names(arg_names), types)) {
#     stop(paste0("The argument list must be: ", arg_names))
#   }
# }


compile_shiny_objects <- function(names, types, defaults) {

  tags <- purrr::map(names, function(name) {

    title <- paste0(tools::toTitleCase(strsplit(name, split = "_")[[1]]), collapse = " ")

    shiny_fun <- types[[name]][["fun"]]

    args <- list(inputId = name, label = title) %>%
      sort_args(shiny_fun, name, defaults)

    print(name)
    print('shiny_fun')
    print(shiny_fun)
    print(args)
    do.call(shiny_fun, args = args)

  })

  # NB: the space before type="number" is required below
  left_panel <- purrr::map(tags, function(x) if(grepl("select", x) | grepl(' type="number"', x) | grepl('type="text"', x)) x else NA)
  left_panel <- left_panel[!is.na(left_panel)]

  right_panel <- purrr::map(tags, function(x) if(grepl("checkbox", x) | grepl("slider", x)) x else NA)
  right_panel <- right_panel[!is.na(right_panel)]


  list("left" = shiny::tagList(left_panel), "right" = shiny::tagList(right_panel))

}


sort_args <- function(args, shiny_fun, name, defaults) {
  input_types <- c(shiny::checkboxInput, shiny::sliderInput,
                   shiny::textInput, shiny::numericInput)

  if(any(purrr::map_lgl(input_types, identical, shiny_fun))) {
    args <- c(args, list("value" = defaults[[name]]))
  }

  if(identical(shiny_fun, shiny::selectInput)) {
    args <- c(args, list("choices" = input_types[[name]][["choices"]]))
  }

  if(identical(shiny_fun, shiny::sliderInput)) {
    warning("sliderInput built only to handle item length. If using new sliders, change this.")
    item_length <- defaults[[name]]
    args <- c(args, list("min" = item_length[[2]]), "max" = item_length[[3]])

  }

  if(identical(shiny_fun, shiny::numericInput)) {
    args <- c(args, list("width" = "100px"))

  }
  args
}


default_if_no_parameter <- function(par, query, defaults) {
  defaults_renested <- defaults %>% nested_list_to_renamed_list(item_to_unnest = "num_items")
  if(is.null(query[[par]])) defaults_renested[[par]] else query[[par]]
}



nested_list_to_renamed_list <- function(l, item_to_unnest) {

  sl <- l[[item_to_unnest]]

  l[[item_to_unnest]] <- NULL

  if(is.call(sl)) {
    res <- eval(sl)
  } else {
    res <- sl
  }

  n <- names(res)

  new_names <- paste0(item_to_unnest, "_", n)

  names(res) <- new_names


  c(l, res)
}




prepare_url_parameters <- function(pars, root_url) {

  c <- purrr::map_chr(1:length(pars), function(i) {
    paste0(names(pars)[i], "=", pars[i])
  })

  url_params <- paste0(c, collapse = "&")

  paste0(root_url, "?", url_params)

}


get_test_fun <- function(test_fun_name) {

  if(test_fun_name == "SAA_standalone") {
    library(SAA)
    args_to_remove <- SAA_args_to_remove
    types <- SAA_input_types
    test_fun <- get(test_fun_name, env = rlang::search_env("package:SAA"))

  } else if(test_fun_name == "PBET_standalone") {
    library(PBET)
    args_to_remove <- PBET_args_to_remove
    types <- PBET_input_types
    test_fun <- get(test_fun_name, env = rlang::search_env("package:PBET"))
  } else if(test_fun_name == "SRT_standalone") {
    library(SRT)
    args_to_remove <- SRT_args_to_remove
    types <- SRT_input_types
    test_fun <- get(test_fun_name, env = rlang::search_env("package:SRT"))
  } else if(test_fun_name == "SST_standalone") {
    library(SST)
    args_to_remove <- SST_args_to_remove
    types <- SST_input_types
    test_fun <- get(test_fun_name, env = rlang::search_env("package:SST"))
  } else if(test_fun_name == "PDT_standalone") {
    library(PDT)
    args_to_remove <- PDT_args_to_remove
    types <- PDT_input_types
    test_fun <- get(test_fun_name, env = rlang::search_env("package:PDT"))
  } else {
    stop("Test function not known")
  }

  list(args_to_remove = args_to_remove, types = types, test_fun = test_fun, defaults = formals(test_fun))
}



