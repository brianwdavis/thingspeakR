library(taskscheduleR)


scriptpath <- "channelGETs.R"

taskscheduler_create(
  taskname = "Thingspeak_scraper", 
  rscript = scriptpath,
  schedule = "HOURLY",
  startdate = format(Sys.Date(), "%m/%d/%Y"),
  debug = T
  )


# taskscheduler_delete(taskname = "Thingspeak_scraper")

# taskscheduler_ls()


