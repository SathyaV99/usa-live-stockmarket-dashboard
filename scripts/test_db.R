source("../config/credentials.R")
library(DBI)
library(RPostgres)

tryCatch({
  con <- dbConnect(RPostgres::Postgres(),
                   dbname = DB_NAME,
                   host = DB_HOST,
                   port = DB_PORT,
                   user = DB_USER,
                   password = DB_PASSWORD,
                   sslmode = "require")
  print("SUCCESS: Connected to Neon Database!")
  
  if (dbExistsTable(con, "predictions_log")) {
    print("Table 'predictions_log' exists.")
    print(dbGetQuery(con, "SELECT COUNT(*) FROM predictions_log"))
  } else {
    print("Table 'predictions_log' does not exist yet (It will be created when the app runs).")
  }
  dbDisconnect(con)
}, error = function(e) {
  print(paste("ERROR connecting:", e$message))
})
