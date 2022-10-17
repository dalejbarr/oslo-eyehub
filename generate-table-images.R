## FUNCTIONS

write_table_image <- function(x, file, 
                              caption = NULL, density = 600L) {
  x %>%
    knitr::kable(caption = caption) %>%
    kableExtra::kable_styling(full_width = FALSE,
                              bootstrap_options = "striped",
                              position = "left") %>%
    kableExtra::column_spec(1, bold = TRUE, monospace = TRUE) %>%
    kableExtra::column_spec(2, monospace = TRUE) %>%
    kableExtra::save_kable(file = file, density = density)
}


codebook_table <- function(x, descriptions = NULL) {
  types <- c("integer" = "<int>",
             "numeric" = "<dbl>",
             "character" = "<chr>",
             "factor" = "<fct>")
  
  dat_cb <- tibble(variable = names(x),
                   type = types[sapply(x, class)])
  
  if (!is.null(descriptions)) {
    if (length(descriptions) != nrow(dat_cb)) {
      stop("length of 'descriptions' (", length(descriptions),
           ") does not match number of variables in table (",
           nrow(dat_cb), ")")
    }
    dat_cb %>%
      mutate(description = descriptions)
  } else {
    dat_cb
  }
}

#######################################################
## packages

library("tidyverse")

#######################################################
## globals

img_path <- "table-images"
sub_id_desc <- "arbitrary value uniquely identifying each subject"
iv_id_desc <- "arbitary value uniquely identifying each version of each stimulus"
i_id_desc <- "arbitrary value uniquely identifying each stimulus set"
s_id_desc <- "arbitrary value uniquely identifying each display screen"
loc_desc <- "arbitrary integer identifying each rectangle"
sound_desc <- "name of the sound file containing the speech"
t_id_desc <- "arbitrary value uniquely identifying each trial within a subject"
ignored <- "(ignored)"

#######################################################
## main script

if (!dir.exists(img_path)) dir.create(img_path)

subjects <- read_csv("data-raw/subjects.csv",
                     col_types = "ic")

codebook_table(subjects,
               c(sub_id_desc,
                 "whether subject is in 'adult' or 'child' group")) %>%
  write_table_image(file = file.path(img_path, "subjects.png"))

locations <- read_csv("data-raw/locations.csv",
                      col_types = "iiiii")

codebook_table(locations,
               c(loc_desc,
                 "horizontal coordinate of top-left corner in pixels",
                 "vertical coordinate of top-left corner in pixels",
                 "horizontal coordinate of bottom-right corner in pixels",
                 "vertical coordinate of bottom-right corner in pixels")) %>%
  write_table_image(file = file.path(img_path, "locations.png"))


stimuli <- read_csv("data-raw/stimuli.csv", col_types = "iiciccc")

codebook_table(stimuli,
               c(iv_id_desc,
                 i_id_desc,
                 "human-friendly identifier of the stimulus set",
                 s_id_desc,
                 "type of competitor, existing or novel",
                 "type of critical image: competitor, unrelated, untrained",
                 sound_desc)) %>%
  write_table_image(file = file.path(img_path, "stimuli.png"))

screens <- read_csv("data-raw/screens.csv",
                    col_types = "iicc")

codebook_table(screens,
               c(s_id_desc,
                 loc_desc,
                 "image's role in the set (target, critical, existing novel)",
                 "name of bitmap file")) %>%
  write_table_image(file.path(img_path, "screens.png"))

speech <- read_csv("data-raw/speech-timings.csv",
                   col_types = "ciii")

codebook_table(speech,
               c(sound_desc,
                 "onset of the definite article [the] in milliseconds from file start",
                 "onset of the noun in milliseconds from file start",
                 "disambiguation point in milliseconds from file start")) %>%
  write_table_image(file.path(img_path, "speech-timings.png"))

trials <- read_csv("data-raw/trials.csv",
                   col_types = "iiiiii")

codebook_table(trials,
               c(sub_id_desc,
                 t_id_desc,
                 iv_id_desc,
                 "was the response accurate? (0: no, 1: yes)",
                 "response time from playback of sound file",
                 "location number that was clicked (1-4)")) %>%
  write_table_image(file.path(img_path, "trials.png"))

eye <- read_tsv("data-raw/adult/sub_001.gazedata",
                col_types = "idiiiiiddddddiddddddiic")

eye %>%
  codebook_table(
    c("arbitrary value uniquely identifying each frame within subject",
      ignored,
      ignored,
      "horizontal point of gaze in pixels",
      "vertical point of gaze in pixels",
      "timestamp in seconds",
      "millisecond portion of timestamp (cycles around)",
      ignored,
      ignored,
      ignored,
      ignored,
      ignored, # DiameterPupilLeftEye
      ignored,
      ignored,
      ignored,
      ignored,
      ignored, # XCameraPosRightEye
      ignored,
      ignored,
      ignored,
      ignored,
      paste(t_id_desc, " (same as t_id)"),
      "phase of the trial (Fixation, Preview, StimSlide)")) %>%
  write_table_image(file.path("table-images", "eye-data.png"))
