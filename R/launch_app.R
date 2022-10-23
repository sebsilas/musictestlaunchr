
# test_launcher_send()


test_launcher_send <- function(root_url = "https://adaptiveeartraining.com/test-demo/") {

  # The static UI elements
  # (some dynamically created ones are in the server function below)
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::titlePanel("musicassessr: App Launcher"),
    shiny::selectInput("test", label = "Test", choices = musicassessr::list_official_tests()),
    shiny::uiOutput("reactive_ui")
  )

  server <- function(input, output) {


    output$reactive_ui <- shiny::renderUI({

      # Sort test function (SAA_standalone vs. PBET_standalone etc.)

      test_fun_name <- reactive({
        input$test %>% sort_dotted_function_call()
      })

      test_fun <- get_test_fun(test_fun_name())

      # As well as the arguments we want users to be able to change (or not) and
      # argument defaults
      remaining_args <- remaining_args(test_fun$test_fun,  test_fun$args_to_remove)
      names <- remaining_args$names
      defaults <- remaining_args$defaults

      # Setup reactives
      pars <- do.call("reactiveValues", defaults)


      objs <- compile_shiny_objects(names = names,
                                    types = test_fun$input_types,
                                    values = defaults)

      # Only update when a value changes

      purrr::walk(names, function(n) {
        observeEvent(input[[n]], {
          shinyjs::hide("app_code")
          output$app_code <- NULL
          pars[[n]] <- input[[n]]
        })
      })

      test_ui <- reactive({
        print(input$test)
        if(input$test == "PBET::PBET_standalone") {
          shiny::selectInput("item_bank", label = "Item Bank", choices = itembankr::list_official_item_banks(default = "WJD"))
        } else {
          shiny::selectInput("item_bank", label = "Item Bank", choices = itembankr::list_official_item_banks())
        }
      })

      # Get R code to launch the app
      shiny::observeEvent(input$get_launch_code, {
        shinyjs::show("app_code")
        code_to_write <- produce_args(test_fun_name(),
                                  names(shiny::reactiveValuesToList(pars)),
                                  shiny::reactiveValuesToList(pars))

        output$app_code <- shiny::renderText(code_to_write)
      })

      # Launch the app in a browser

      shiny::observeEvent(input$launch_app, {

        url <- prepare_url_parameters(shiny::reactiveValuesToList(pars), root_url, test_fun_name())

        shinyjs::runjs(paste0('window.open(\"',url,'\", "_blank").focus();'))
      })

      # The (dynamically created) UI elements
      shiny::tags$div(

        shinyjs::useShinyjs(),

        shiny::tags$head(shiny::tags$style(css_send)),


        shiny::fluidRow(
          shiny::column(4,

                if(input$test != "PDT::PDT_standalone") {
                  tags$div(

                    test_ui(),

                    shiny::selectInput("melody_sound",
                                        label = "Playback Sound",
                                        choices = sound_types,
                                        selected = "piano"
                                        ))},

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



sound_types <- as.list(musicassessr::list_tone_sound_types()) %>%
  stats::setNames(object = ., nm = unlist(.) %>% stringr::str_replace('[-_]', " ") %>% stringr::str_to_title())

sort_dotted_function_call <- function(str, return_only_fun_name = TRUE, prefix_package_colon = FALSE) {
  # separate a call like Berkowitz::Berkowitz into components
  s <- strsplit(str, split = "::") %>%
    purrr::pluck(1)

  if(return_only_fun_name) {
    s <- s %>% purrr::pluck(2)
  } else {
    s <- as.list(s)
    names(s) <- c("package", "fun")
    if(prefix_package_colon) {
      s$package <- paste0('package:', s$package)
    }
  }
  s
}

get_test_fun_name <- function(test_fun) {
  test_fun_name <- as.character(substitute(test_fun))
  test_fun_name <- paste0(test_fun_name[2], test_fun_name[1], test_fun_name[3])
}

nest_list <- function(l, nest_name) {
  n <- names(l)
  names(l) <- stringr::str_remove(n, nest_name)
  l
}


sort_arg_names_and_vals <- function(arg_names_and_vals, arg_names, test_fun_name) {
  if(test_fun_name != "PDT_standalone") {
    # factor all this.. also in receiver_app.R around url_params
    ix <- grep("num_items_", arg_names)
    num_item_names <- arg_names[ix]

    # for num_items
    num_item_arg_names_and_vals <- arg_names_and_vals[num_item_names] %>%
      nest_list("num_items_")
    arg_names <- arg_names[-ix]
    arg_names_and_vals <- arg_names_and_vals[-ix]
    arg_names_and_vals$num_items <- num_item_arg_names_and_vals

    # them arrhythmic/rhythmic
    new_item_names <- names(arg_names_and_vals$num_items)

    ix2 <- grep("arrhythmic_", new_item_names)
    arrhythmic_names <- new_item_names[ix2]
    arrhythmic_arg_names_and_vals <- arg_names_and_vals$num_items[arrhythmic_names] %>%
      nest_list("arrhythmic_")
    new_item_names2 <- new_item_names[-ix2]

    ix3 <- grep("rhythmic_", new_item_names2)
    rhythmic_names <- new_item_names2[ix3]
    rhythmic_arg_names_and_vals <- arg_names_and_vals$num_items[rhythmic_names] %>%
      nest_list("rhythmic_")
    new_item_names3 <- new_item_names2[-ix3]


    arg_names_and_vals$num_items[c("rhythmic_key_hard",
                                   "rhythmic_key_easy",
                                   "arrhythmic_key_hard",
                                   "arrhythmic_key_easy")] <- NULL

    # wjd
    if(test_fun_name == "PBET_standalone") {
      #browser()
      ix4 <- grep("wjd_", new_item_names3)
      wjd_names <- new_item_names3[ix4]



      wjd_arg_names_and_vals <- arg_names_and_vals$num_items[wjd_names] %>%
        nest_list("wjd_audio_")

      arg_names_and_vals$num_items$arrhythmic <- arrhythmic_arg_names_and_vals
      arg_names_and_vals$num_items$rhythmic <- rhythmic_arg_names_and_vals
      arg_names_and_vals$num_items$wjd_audio <- wjd_arg_names_and_vals

      arg_names_and_vals$num_items[c("wjd_audio_key_hard",
                                     "wjd_audio_key_easy",
                                     "wjd_audio_rhythmic_key_hard",
                                     "wjd_audio_rhythmic_key_easy",
                                     "wjd_audio_arrhythmic_key_hard",
                                     "wjd_audio_arrhythmic_key_easy")] <- NULL
    }

  }
  arg_names_and_vals
}

produce_args <- function(test_fun_name, arg_names, arg_names_and_vals) {

  arg_names_and_vals <- sort_arg_names_and_vals(arg_names_and_vals, arg_names, test_fun_name)


  arg_list <- purrr::map2_chr(names(arg_names_and_vals), arg_names_and_vals, function(arg_name, val) {
    paste0(arg_name, ' = ', deparse(val, width.cutoff = 500L))
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

  if(identical(test_fun, PBET::PBET_standalone)) {

    arg_names_and_vals <- arg_names_and_vals %>%
      nested_list_to_renamed_list(item_to_unnest = "num_items_arrhythmic") %>%
      nested_list_to_renamed_list(item_to_unnest = "num_items_rhythmic") %>%
      nested_list_to_renamed_list(item_to_unnest = "num_items_wjd_audio")
  }

  # remove "..."

  arg_names_and_vals$... <- NULL

  arg_names <- names(arg_names_and_vals)

  if(!is.null(args_to_remove)) {
    arg_names_and_vals <- arg_names_and_vals[!arg_names %in% args_to_remove]
    arg_names <- names(arg_names_and_vals)
  }

  if(!is.null(arg_names_and_vals$item_bank) & is.call(arg_names_and_vals$item_bank)) {
    arg_names_and_vals$item_bank <- deparse(arg_names_and_vals$item_bank)
  }

  if(!is.null(arg_names_and_vals$melody_length) & is.call(arg_names_and_vals$melody_length)) {
    arg_names_and_vals$melody_length <- eval(arg_names_and_vals$melody_length)
  }


  list(names = arg_names, defaults = arg_names_and_vals)
}



compile_shiny_objects <- function(names, types, values) {

  if(any(names == "item_bank")) {
    names <- names[-which(names == "item_bank")] # we render this statically
  }

  tags <- purrr::map(names, function(name) {

    title <- paste0(tools::toTitleCase(strsplit(name, split = "_")[[1]]), collapse = " ")


    shiny_fun <- types[[name]][["fun"]]

    args <- list(inputId = name, label = title) %>%
      sort_args(shiny_fun, name, values)

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

  if(purrr::some(input_types, identical, shiny_fun)) {
    args <- c(args, list("value" = defaults[[name]]))
  }

  if(identical(shiny_fun, shiny::selectInput)) {
    args <- c(args, list("choices" = input_types[[name]][["choices"]]))
  }

  if(identical(shiny_fun, shiny::sliderInput)) {
    warning("sliderInput built only to handle melody_length. If using new sliders, change this.")
    item_length <- defaults[[name]]
    args <- c(args, list("min" = item_length[1]), "max" = item_length[2])
    #args <- c(args, list("min" = item_length[[2]]), "max" = item_length[[3]])

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




prepare_url_parameters <- function(pars, root_url, test_fun_name) {

  c <- purrr::map_chr(1:length(pars), function(i) {
    paste0(names(pars)[i], "=", pars[i])
  })

  tf <- paste0("test_fun_name=", test_fun_name)

  c <- c(tf, c)

  url_params <- paste0(c, collapse = "&")

  paste0(root_url, "?", url_params)

}


get_test_fun <- function(test_fun_name) {

  if(test_fun_name == "SAA_standalone") {
    library(SAA)
    args_to_remove <- SAA_args_to_remove
    types <- SAA_types
    input_types <- SAA_input_types
    test_fun <- get(test_fun_name, env = rlang::search_env("package:SAA"))

  } else if(test_fun_name == "PBET_standalone") {
    library(PBET)
    args_to_remove <- PBET_args_to_remove
    types <- PBET_types
    input_types <- PBET_input_types
    test_fun <- get(test_fun_name, env = rlang::search_env("package:PBET"))
  } else if(test_fun_name == "SRT_standalone") {
    warning("Using SST in place of SRT for now.")
    library(SST)
    args_to_remove <- SRT_args_to_remove
    types <- SRT_types
    input_types <- SRT_input_types
    test_fun <- get("SST_standalone", env = rlang::search_env("package:SST"))
  } else if(test_fun_name == "SST_standalone") {
    library(SST)
    args_to_remove <- SST_args_to_remove
    types <- SST_types
    input_types <- SST_input_types
    test_fun <- get(test_fun_name, env = rlang::search_env("package:SST"))
  } else if(test_fun_name == "PDT_standalone") {
    library(PDT)
    args_to_remove <- PDT_args_to_remove
    types <- PDT_types
    input_types <- PDT_input_types
    test_fun <- get(test_fun_name, env = rlang::search_env("package:PDT"))
  } else {
    stop("Test function not known")
  }

  list(args_to_remove = args_to_remove,
       types = types,
       input_types = input_types,
       test_fun = test_fun,
       defaults = formals(test_fun))
}


get_item_bank <- function(item_bank_name) {

  item_bank <- sort_dotted_function_call(item_bank_name, return_only_fun_name = FALSE, prefix_package_colon = TRUE)

  if(item_bank_name == "Berkowitz::Berkowitz") {
    library(Berkowitz)
    item_bank_fun <- get(item_bank$fun, env = rlang::search_env(item_bank$package))
  } else if(test_fun_name == "WJD::WJD") {
    library(WJD)
    item_bank_fun <- get(item_bank$fun, env = rlang::search_env(item_bank$package))
  } else if(test_fun_name == "Slonimsky::Slonimsky") {
    library(Slonimsky)
    item_bank_fun <- get(item_bank$fun, env = rlang::search_env(item_bank$package))
  } else {
    stop("Item bank not known. Is it an official musicassessr item bank?
         Check with musicassessr::list_official_tests()")
  }

  item_bank_fun
}



css_send <- "#app_code { font-size:12px; font-style:italic;overflow-y:scroll; width: 400px;
                                      background: ghostwhite; border: solid 1px #f1e9f5; border-radius: 3px;
                                      margin: 10px; padding: 10px; visibility: hidden;}"






