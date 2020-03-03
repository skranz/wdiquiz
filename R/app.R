examples_wdi_quiz_app = function() {
  db.file = "D:/libraries/dataquiz/wdi/wdi.sqlite"
  options(warn=1)
  app = wdi_quiz_app(db.file)
  viewApp(app)
}

wdi_quiz_app = function(db.file) {
  app = eventsApp()
  app$ui = fluidPage(
    uiOutput("mainUI"),
    #fluidRow(
      actionButton("nextBtn","New quiz")
    #)
  )
  buttonHandler("nextBtn",function(...) {
    set_random_quiz(country1="DEU", country2="FRA")
  })

  schema.file = system.file("schemas/wdi.yaml", package = "wdiquiz")
  db = dbmisc::dbConnectSQLiteWithSchema(db.file,schema.file = schema.file)
  app$glob$db = db
  series.file = system.file("data/series.Rds", package = "wdiquiz")
  app$glob$series = readRDS(series.file)

  appInitHandler(function(app,...) {
    restore.point("appInitHandler")
    country1 = "DEU"
    country2 = "FRA"
    set_quiz(series_code="AG.LND.CROP.ZS", country1, country2)

  })
  app
}

set_random_quiz = function(series_code = sample(series$series_code,1), country1="DEU",country2="FRA", series = getApp()$glob$series) {
  restore.point("set_random_quiz")
  set_quiz(series_code, country1, country2)
}

set_quiz = function(series_code, country1, country2, inds = sample.int(2), db=getApp()$glob$db,...) {
  restore.point("set_quiz")
  countries = c(country1, country2)[inds]
  dat1 = dbGet(db,"data",params = list(series_code=series_code, country_code=countries[1]))
  dat2 = dbGet(db,"data",params = list(series_code=series_code, country_code=countries[2]))

  si = series_info(series_code)

  library(svglite)
  plot = ggplot(mapping = aes(x=year,y=value)) +
    geom_line(data=dat1, color="red") +
    geom_line(data=dat2, color="blue") +
    xlab("Year") + ylab("")

  s <- svgstring(width = 6, height=3, pointsize=16,standalone=TRUE);
  print(plot)
  svg = merge.lines(as.character(s()))
  dev.off()
  #svg = sub("viewBox=","preserveAspectRatio='none' viewBox=", svg,fixed=TRUE)
  cat("\nsvg:\n\n")
  cat(svg)

  b64 = jsonlite::base64_enc(svg)
  ui = tagList(
      h3(si$indicator_name, style="padding-left: 2em; text-align:left; margin-left: 0em;"),
      tags$img(style="width: 90%; max-width: 60em; margin-left: 1em;", src = paste0("data:image/svg+xml;base64,",b64)),
      #tags$img(style="width: 90%; max-height: 24em; max-width: 60em; margin-left: 2em; margin-right: auto;", src = paste0("data:image/svg+xml;base64,",b64)),
      br(),
      actionButton("blueBtn","Blue"),
      actionButton("redBtn","Red")
  )
  setUI("mainUI",ui)
  dsetUI("mainUI",ui)

}

series_info = function(series_code, series = getApp()$glob$series) {
  series[series$series_code == series_code,]

}
