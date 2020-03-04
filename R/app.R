examples_wdi_quiz_app = function() {
  db.file = "D:/libraries/dataquiz/wdi/wdi.sqlite"
  options(warn=1)
  app = wdi_quiz_app(db.file, country2="JPN")
  viewApp(app)
}

wdi_quiz_app = function(db.file, country1="DEU", country2="FRA") {
  app = eventsApp()
  app$ui = fluidPage(
    uiOutput("mainUI")
  )
  buttonHandler("nextBtn",function(...) {
    set_next_quiz()
  })

  buttonHandler("redBtn", function(...) process_answer("red"))
  buttonHandler("blueBtn", function(...) process_answer("blue"))



  schema.file = system.file("schemas/wdi.yaml", package = "wdiquiz")
  db = dbmisc::dbConnectSQLiteWithSchema(db.file,schema.file = schema.file)
  app$glob$db = db
  series.file = system.file("data/series.Rds", package = "wdiquiz")
  app$glob$all_series = readRDS(series.file)
  app$glob$series = filter(app$glob$all_series, used==TRUE)

  app$country1 = country1
  app$country2 = country2
  app$quiz_type = "2c"

  appInitHandler(function(app,...) {
    restore.point("appInitHandler")
    set_next_quiz()
  })
  app
}

set_next_quiz = function(app=getApp()) {
  if (app$quiz_type == "2c") {
    set_2c_random_quiz()
  } else {
    stop(paste0("Unknown quiz type ", app$quiz_type))
  }
}

find_2c_series = function(country1=app$country1, country2=app$country2, db=app$glob$db, series=app$glob$series, app=getApp(), min_distinct=2) {
  restore.point("find_2c_series")

  sql = "select series_code from country_series where country_code == :country AND num_distinct >= :min_distinct"
  se1 = dbGet(db,sql=sql ,params = list(country=country1,min_distinct=min_distinct ))$series_code

  se2 = dbGet(db, sql = sql,params = list(country=country2,min_distinct=min_distinct ))$series_code

  codes = intersect(intersect(se1,se2),series$series_code)
  codes

}

set_2c_random_quiz = function(country1=app$country1,country2=app$country2,series_code = NULL, app=getApp()) {
  restore.point("set_2c_random_quiz")
  if (is.null(series_code)) {
    if (length(app$series_codes)==0) {
      app$series_codes = find_2c_series(country1, country2)
    }
    series_code = sample(app$series_codes, 1)
  }
  # Remove current series_code from list in app
  app$series_codes = setdiff(app$series_codes, series_code)

  set_quiz(series_code, country1, country2)
}

process_answer = function(click_color, app=getApp(),...) {
  restore.point("process_answer")

  correct = (click_color == app$color1)
  cat("\nCorrect = ", correct)

  if (correct) {
    msg = paste0("Correct, ",app$country1, " is shown ", app$color1, ".")
  } else {
    msg = paste0("Wrong, ",app$country1, " is shown ", app$color1, ".")
  }
  bg = if (app$color1=="blue") "#9999ff" else "#ff9999"
  #btn = simpleButton("nextBtn",paste0("<p>",msg, " Click to continue.</p>"), style=paste0("margin-left: 2em; width: 90%; max-width: 60em; background-color: ", bg,";"))
  #html = paste0(msg,"\n", as.character(btn))
  btn = paste0('<button id="nextBtn" style="margin-left: 2em; width: 90%; max-width: 60em; word-break: keep-all; background-color: ',bg,';" type="button" class="btn btn-default action-button ">',msg, '<br> Click to continue.</button>')
  html = as.character(btn)
  shinyEvents::setInnerHTML("result_div", html=html)
  shinyEvents::setHtmlHide("choice_div")

}

set_quiz = function(series_code, country1, country2, inds = sample.int(2), db=getApp()$glob$db,app=getApp(),...) {
  restore.point("set_quiz")

  app$country1 = country1
  app$country2 = country2
  app$color1 = c("red","blue")[inds[1]]

  countries = c(country1, country2)[inds]
  dat1 = dbGet(db,"data",params = list(series_code=series_code, country_code=countries[1]))
  dat2 = dbGet(db,"data",params = list(series_code=series_code, country_code=countries[2]))

  si = series_info(series_code)


  library(svglite)
  plot = ggplot(mapping = aes(x=year,y=value)) +
    geom_line(data=dat1, color="red") +
    geom_line(data=dat2, color="blue") +
    xlab("") + ylab("")

  labeller = get_scale_labeller(max(dat1$value,dat2$value, na.rm=TRUE))
  if (!is.null(labeller)) {
    plot = plot + scale_y_continuous(labels = labeller)
  }

  s <- svgstring(width = 6, height=3, pointsize=16,standalone=TRUE);
  print(plot)
  svg = merge.lines(as.character(s()))
  dev.off()
  #svg = sub("viewBox=","preserveAspectRatio='none' viewBox=", svg,fixed=TRUE)
  #cat("\nsvg:\n\n")
  #cat(svg)

  b64 = jsonlite::base64_enc(svg)
  style="white-space: normal; margin-bottom: 0.5em; background-color: #ff9999; width: 100%; margin-left: 1em;"
  redBtn = simpleButton(id=paste0("redBtn"),label = paste0(country1," is red"), style=style)
  style="white-space: normal; margin-bottom: 0.5em; background-color: #9999ff;; width: 100%; margin-left: 2em;"
  blueBtn = simpleButton(id=paste0("blueBtn"),label = paste0(country1," is blue"), style=style)

  ui = tagList(
      h4(paste0(si$indicator_name,": ", country1, " vs ", country2), style="padding-left: 1em; text-align:left; margin-left: 0em;"),
      tags$img(style="width: 90%; max-width: 60em; margin-left: 0em;", src = paste0("data:image/svg+xml;base64,",b64)),
      br(),
      div(id="result_div", style="width: 100%; padding-left: 2em;  max-width: 60em;"),
      div(id="choice_div",
        #p(style="padding-left: 2em;", paste0("Which color shows the data for ", country1, " (instead of ", country2,")?")),
        tags$table(style="width: 90%; padding-left: 2em;  max-width: 60em;", tags$tr(style="width: 100%",
          tags$td(style="padding-left: 1em;",
            redBtn
          ),
          tags$td(
            blueBtn
          )
        ))
      ),
      div(id="descr_div", style="width: 100%; padding-left: 2em;  max-width: 60em;",
        h5("Description:"),
        HTML(paste0("<p>",si$descr,"</p>"))
      )
  )
  setUI("mainUI",ui)
  dsetUI("mainUI",ui)

}

get_scale_labeller = function(values) {
  mv = max(values, na.rm=TRUE)
  if (mv<=1000)
    return(NULL)
  if (mv <= 1e6)
    return(comma_format())
  if (mv <= 1e10)
    return(unit_format(unit="M", scale=1/1e6))

  return(unit_format(unit="B", scale=1/1e9))


}

series_info = function(series_code, series = getApp()$glob$series, db=app$glob$db, add.descr=TRUE) {
  series = dbGet(db,"series",list(series_code=series_code))
  if (add.descr) {
    series$descr = paste0(
      if (!is.empty(series$long_definition)) series$long_definition else series$short_definition,
      "<br>Topic: <i>",series$topic,"</i>"
    )
    if (!is.empty(series$source)) {
      series$descr = paste0(series$descr, "<br>Source: ", series$source)
    }
  }
  series
}
