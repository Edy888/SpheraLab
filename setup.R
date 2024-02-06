################################ ---Read and prepare data ---###################




imm_instr <- read_csv("test_data/imm_mdx_all.csv", 
                      col_types = cols(id = col_integer(), 
                                       result = col_number(), date_order = col_date(format = "%m/%d/%Y"), 
                                       date_authorization = col_date(format = "%m/%d/%Y"), 
                                       years = col_number(), month = col_number(), 
                                       days = col_number()), na = "NA")



imm_instr <- imm_instr |> 
  mutate(weekday = weekdays(date_order, abbreviate = TRUE)) |>
  relocate(weekday, .before = date_order)




acl_instr <- read_csv("test_data/top_mdx_all.csv", 
                      col_types = cols(id = col_integer(), 
                                       result = col_number(), date_order = col_date(format = "%m/%d/%Y"), 
                                       date_authorization = col_date(format = "%m/%d/%Y"), 
                                       years = col_number(), month = col_number(), 
                                       days = col_number()))


acl_instr <- acl_instr <- acl_instr |> 
  dplyr::filter(!is.na(result))


acl_instr <- acl_instr |> 
  mutate(weekday = format(date_order, "%a")) |>  # или weekdays(date_order)
  relocate(weekday, .before = date_order)





####################---Создание боковой панели---###############################

instrument <- c("Immulite 2000Xpi", "ACL Top 750")
d_w_m <- c("day", "week", "month")

sidebar_content <- list(
  selectInput("instrument",
              "Select Instrument",
              choices = instrument,
              selected = "",
              multiple  = FALSE),
  
  radioButtons("d_w_m",
               "Select Time Interval",
               choices = c("day", "week", "month"),
               selected = "day")
)



#################### ---Reactive expression--- #################################


