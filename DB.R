library(RSQLite)
db <- dbConnect(SQLite(), "applications.db")
dbExecute(db, "
  CREATE TABLE IF NOT EXISTS Applications (
    ID INTEGER PRIMARY KEY,
    Name TEXT,
    Skills TEXT,
    Location TEXT,
    Experience REAL,
    SkillCount INTEGER
  )
")
dbDisconnect(db)
