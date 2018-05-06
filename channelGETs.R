message(Sys.time())

suppressPackageStartupMessages({
  library(httr)
  library(purrr)
  library(dplyr)
  library(readr)
  library(stringr)
  })

orig <- getwd()

# setwd("local_path")

# Start logging
ts_start <- Sys.time()


safe_GET <- function(url) RETRY("GET", url)

chans <- 
  list(
    # MD = "secret_key",
    # NC = "secret_key",
    # GA = "secret_key"
    ) %>% 
  map(~paste0("https://api.thingspeak.com/channels.json?api_key=", .x)) %>% 
  map(possibly(safe_GET, otherwise = NA))

# Store status of all three users' accounts
users_status <- 
  map_chr(chans, possibly(status_code, otherwise = "Bad userkey?")) %>% 
  imap_chr(~paste(.y, .x, sep = ":")) 


# Catch stray HTML files for diagnosing
# Always use with imap
safe_json_parser <- function(x, y = "unknown") {
  if (is.null(x)) return(NULL)

  x_parsed <- safely(jsonlite::fromJSON)(x)
  
  if (is.null(x_parsed$result)) {
    write(x, file = paste(format(Sys.time(), "%F %H.%M.%OS3"), y, "json error.txt"))
  }
  
  return(x_parsed$result)
}


# Parse a dataframe of each state's channels
#   Bind them all
#   Extract the second key from the api_key list-column
#   (structure is api_key, write_flag [T],
#                 api_key, write_flag [F])

chans_apis <- 
  map(chans, 
      possibly(content, otherwise = NULL), 
      as = "text") %>% 
  imap(safe_json_parser) %>% 
  bind_rows(.id = "state") %>% 
  mutate(api_keys = map(api_keys, "api_key"),
         read_api = map(api_keys, 2))

# Store a copy of the keys for sharing  
# chans_apis %>% 
#   mutate(api_keys = map(api_keys, paste, collapse = ", ")) %>% 
#   tidyr::separate(api_keys, c("write_key", "read_key")) %>% 
#   select(-read_api, -tags) %>% 
#   write_csv("./api_keys.csv")


# GET data from each channel
chan_gets <- 
paste0("https://api.thingspeak.com/channels/", 
       chans_apis$id,
       "/feeds.json?api_key=",
       chans_apis$read_api,
       "&results=1") %>% 
  set_names(chans_apis$name) %>% 
  map(possibly(safe_GET, otherwise = NA))

# Store the status of each channel GET
chans_status <- 
  chan_gets %>% 
  map(possibly(status_code, otherwise = "Bad readkey?")) %>% 
  imap_chr(~paste(.y, .x, sep = ":"))

# Store raw text backups
chan_gets %>% 
  map(content, as = "text") %>% 
  iwalk(~write_lines(.x, 
                     paste0("./raw_text/", .y, ".txt"), 
                     append = T))

# to recover, map this function over a list of filenames:
# {read_lines(filepath) %>% imap(safe_json_parser) %>% map("feeds") %>% bind_rows()}
# 
# To examine possible non-JSON entries, you can use this code:
# But be careful printing it, this can be a very slow process and may crash R
#   Or may return external pointers, not sure why

# x_text <- list.files("./raw_text", full.names = T) %>%
#   set_names(., basename(.)) %>%
#   map(read_lines)
# 
# DON'T PRINT x_text without:
#   options(max.print = 2)
#   x_text
# 
# x_text_parsed <- x_text %>%
#   map(unique) %>% 
#   map(~map(.x, ~possibly(xml2::read_html, otherwise = NULL)(.x)) %>% compact()) %>%
#   compact()



# Read contents in, 
#   parse as JSON (simplify* = T, the default for content() is  F),
#   keep only the feeds (tossing channel info),
#   extract embedded dataframes, write CSVs
safe_write <- possibly(write_csv, otherwise = NULL)

chan_gets %>% 
  map(content, as = "text") %>% 
  imap(safe_json_parser) %>% 
  map("feeds") %>% 
  iwalk(~safe_write(.x, paste0("./raw_csv/", .y, " newest.csv")))




# Given the newly created CSV files ("newest"), look for older ones with the
# same stem. If that base file is found, try to join them up and save under the 
# base stem name. Otherwise, rename the new one to be the base file.

csv_joiner <- function(filename) {
  
  oldfilename <- str_replace_all(filename, " newest", "")
  
  safe_read <- possibly(read_csv, otherwise = data_frame(entry_id = -999))
  
  nf <- safe_read(filename, col_types = cols(entry_id = "d", .default = "c"))
  of <- safe_read(oldfilename, col_types = cols(entry_id = "d", .default = "c"))
  
  if (nrow(nf) == 0) {
    file.remove(filename)
  }
  
  if (nrow(of) == 0) {
    file.rename(filename, oldfilename)
  }
  
  if (nrow(nf) > 0 & nrow(of) > 0) {
    full_join(of, nf) %>% 
      write_csv(oldfilename)
    file.remove(filename)
  } 
}


list.files("./raw_csv/", " newest.csv", full.names = T) %>% 
  walk(csv_joiner)


ts_finish <- Sys.time()

data_frame(
  start = ts_start,
  finish = ts_finish,
  users =  paste(users_status, collapse = "; "),
  n = length(chan_gets),
  chans =  paste(chans_status, collapse = "; ")
) %>% 
  write_csv("./read_log.csv", append = T)


http_errors <- 
  c(users_status, chans_status) %>% 
  discard(~str_detect(.x, "200"))

json_errors <- 
  c(chans, chan_gets) %>% 
  map(http_type) %>% 
  imap_chr(~paste(.y, .x, sep = ":")) %>% 
  discard(~str_detect(.x, "json")) 

error_log <- 
  data_frame(time = ts_start,
             errors = c(http_errors, json_errors))

# send a message to Brian's phone via the IFTTT app
if (nrow(error_log) > 0) {
  
  write_csv(error_log, "./error_log.csv", append = T)
  
  POST("https://maker.ifttt.com/trigger/RichPushfromR/with/key/<secret_key>",
       body = list("value1" = nrow(error_log)), encode = "json") 
}
  
  
setwd(orig)
