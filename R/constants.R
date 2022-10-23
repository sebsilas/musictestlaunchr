
general_musicassessr_types <- list(
                  "num_items_rhythmic" = "numeric",
                  "num_items_arrhythmic" = "numeric",
                  "item_bank" = "function",
                  "demographics" = "logical",
                  "feedback" = "logical",
                  "SNR_test" = "logical",
                  "get_range" = "logical",
                  "examples" = "numeric",
                  "final_results" = "logical",
                  "gold_msi" = "logical",
                  "melody_length" = "numeric",
                  "melody_sound" = "character",
                  "adjust_range" = "logical",
                  "test_name" = "character",
                  "show_socials" = "logical",
                  "headphones_test" = "logical",
                  "microphone_test" = "logical",
                  "allow_repeat_SNR_tests" = "logical",
                  "max_goes" = "numeric",
                  "max_goes_forced" = "logical",
                  "long_tone_trials_as_screening" = "logical",
                  "long_tone_trials_as_screening_failure_page" = "character",
                  "success_on_completion_page" = "character",
                  "concise_wording" = "logical",
                  "skip_setup" = "logical")


general_musicassessr_args_to_remove <- c(
                        "demo", "admin_password", "absolute_url",
                        "musicassessr_aws", "store_results_in_db", "test_username",
                        "with_final_page", "get_user_info", "append_trial_block_before", "append_trial_block_after",
                        "stop_recording_after", "app_name", "full_screen"
                        )

general_musicassessr_input_types <- list(
                        "num_items_arrhythmic" = list("fun" = shiny::numericInput),
                        "num_items_rhythmic" = list("fun" = shiny::numericInput),
                        "item_bank" = list("fun" = shiny::selectInput),
                        "demographics" = list("fun" = shiny::checkboxInput),
                        "feedback" = list("fun" = shiny::checkboxInput),
                        "SNR_test" = list("fun" = shiny::checkboxInput),
                        "get_range" = list("fun" = shiny::checkboxInput),
                        "examples" = list("fun" = shiny::numericInput),
                        "final_results" = list("fun" = shiny::checkboxInput),
                        "gold_msi" = list("fun" = shiny::checkboxInput),
                        "melody_length" = list("fun" = shiny::sliderInput),
                        "melody_sound" = list("fun" = shiny::selectInput),
                        "adjust_range" = list("fun" = shiny::checkboxInput),
                        "test_name" = list("fun" = shiny::textInput),
                        "show_socials" = list("fun" = shiny::checkboxInput),
                        "headphones_test" = list("fun" = shiny::checkboxInput),
                        "microphone_test" = list("fun" = shiny::checkboxInput),
                        "allow_repeat_SNR_tests" = list("fun" = shiny::checkboxInput),
                        "max_goes" = list("fun" = shiny::numericInput, "value" = 3),
                        "max_goes_forced" = list("fun" = shiny::checkboxInput),
                        "success_on_completion_page" = list("fun" = shiny::textInput),
                        "skip_setup" = list("fun" = shiny::checkboxInput),
                        "concise_wording" = list("fun" = shiny::checkboxInput))


# Extra SAA

SAA_types <- c(general_musicassessr_types,
                list("long_tone_trials_as_screening" = "logical",
                     "num_items_long_tones" = "numeric",
                    "long_tone_trials_as_screening_failure_page" = "character",
                    "concise_wording" = "logical"))


SAA_input_types <- c(general_musicassessr_input_types,
                list(
   "num_items_long_tones" = list("fun" = shiny::numericInput),
  "long_tone_trials_as_screening" = list("fun" = shiny::checkboxInput),
  "long_tone_trials_as_screening_failure_page" = list("fun" = shiny::textInput)
  ))


SAA_args_to_remove <- c(general_musicassessr_args_to_remove,
                        "num_items_wjd_audio")


# Extra PBET

PBET_types <- c(general_musicassessr_types,
                list(
                "num_items_arrhythmic_key_easy" = 'numeric',
                "num_items_arrhythmic_key_hard" = 'numeric',
                "num_items_rhythmic_key_easy" = 'numeric',
                "num_items_rhythmic_key_hard" = 'numeric',
                "num_items_interval_perception" = "numeric",
                "num_items_find_this_note" = "numeric",
                "num_items_wjd_audio_key_easy" = "numeric",
                "num_items_wjd_audio_key_hard" = "numeric")
                )


PBET_input_types <- c(general_musicassessr_input_types,
                      list(
                        "give_first_melody_note" = list("fun" = shiny::checkboxInput),
                      "input" = list("fun" = shiny::selectInput),
                      "num_items_arrhythmic_key_easy" = list("fun" = shiny::numericInput),
                      "num_items_arrhythmic_key_hard" = list("fun" = shiny::numericInput),
                      "num_items_rhythmic_key_easy" = list("fun" = shiny::numericInput),
                      "num_items_rhythmic_key_hard" = list("fun" = shiny::numericInput),
                      "num_items_interval_perception" = list("fun" = shiny::numericInput),
                      "num_items_find_this_note" = list("fun" = shiny::numericInput),
                      "num_items_wjd_audio_key_easy" = list("fun" = shiny::numericInput),
                      "num_items_wjd_audio_key_hard" = list("fun" = shiny::numericInput)
                      ))


PBET_args_to_remove <- c(general_musicassessr_args_to_remove,
                         "item_characteristics_sampler_function",
                         "get_trial_characteristics_function",
                         "get_answer_function_midi",
                         "get_answer_function_audio",
                         "get_self_chosen_anonymous_id",
                         "sampler_function_arrhythmic",
                         "sampler_function_rhythmic",
                         "main_module_name",
                         "num_items_wjd_audio")



# Extra SST

SST_types <- c(general_musicassessr_types,
               list("num_items_long_tones" = "numeric"))



SST_input_types <- c(general_musicassessr_input_types,
                     list(
                       "num_items_long_tones" = list("fun" = shiny::numericInput),
                       "long_tone_trials_as_screening" = list("fun" = shiny::checkboxInput),
                     "long_tone_trials_as_screening_failure_page" = list("fun" = shiny::textInput)))


SST_args_to_remove <- general_musicassessr_args_to_remove





# Extra SRT

# NB! Eventually we need to update these to remove some of the arguments (and perhaps include some)
# We are copying the SST stuff for now, but the SRT will be different

SRT_types <- c(general_musicassessr_types,
               list("num_items_long_tones" = "numeric"))



SRT_input_types <- c(general_musicassessr_input_types,
                     list(
                       "num_items_long_tones" = list("fun" = shiny::numericInput),
                       "long_tone_trials_as_screening" = list("fun" = shiny::checkboxInput),
                       "long_tone_trials_as_screening_failure_page" = list("fun" = shiny::textInput)))

SRT_args_to_remove <- general_musicassessr_args_to_remove

# Extra PDT

PDT_args_to_remove <- c("admin_password", "researcher_email", "item_bank")

PDT_types <- list("title" = "character",
                  "num_items" = "numeric",
                  "demo" = "logical")

PDT_input_types <- list("demo" = list("fun" = shiny::checkboxInput),
                      "title" = list("fun" = shiny::textInput),
                      "num_items" = list("fun" = shiny::numericInput))

