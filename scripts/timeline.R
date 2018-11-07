require(DBI)
require(odbc)
require(tidyverse)
require(dygraphs)

if (exists(con)) rm(con)
con <- dbConnect(odbc(), "Freezerworks64")

timeline <- dbGetQuery(con, "SELECT PK_AliquotUID, Date_In, Date_Out FROM Aliquots")

dbDisconnect(con)
rm(con)

timeline <- timeline %>% 
  filter(!is.na(DATE_IN),
         DATE_IN > "1960-01-01") %>%
  mutate(residence = DATE_OUT - DATE_IN) %>%
  arrange(DATE_IN)

timeIN <- timeline %>%
  count(DATE_IN)
timeIN <- data.frame(row.names = timeIN$DATE_IN,
                     n = cumsum(timeIN$n))

timeOUT <- timeline %>%
  count(DATE_OUT)
timeOUT <- data.frame(row.names = timeOUT$DATE_OUT,
                      n = cumsum(timeOUT$n))

timeline_plot <- rbind(timeIN, timeOUT) %>% arrange()

residence_time <- summary(timeline$residence)