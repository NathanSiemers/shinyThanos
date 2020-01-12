library(shiny)
library(tidyverse)
library(ggthemes)
if(! require('shinyCustom')) {
    devtools::install_github('homerhanumat/shinyCustom')
}
library(shinyCustom)
library(shinycssloaders)
unique_categorical = 13  ## less than this and we make categories instead of sliders
max_hist_cat = 20  ## above this, we don't show categorical histograms
################################################################
## testing dataset 
data = as.data.frame(storms)
data_selected = c('category', 'lat', 'long')
ind  = sapply(storms, is.numeric)
data_categorical = colnames(data)[ ! ind ]
data_facet = 'category'
data_description =    
    'This data is a subset of the NOAA Atlantic hurricane database best track data, http://www.nhc.noaa.gov/data/#hurdat.
\n\n
The data includes the positions and attributes of 198 tropical storms, measured every six hours during the lifetime of a storm.'
################################################################
## my theme
theme_hist = theme_gdocs() + theme(legend.position = "none") + theme(
                                       plot.title = element_text(size = 0, hjust = 0), axis.text.y =
                                           element_text(size = 0, hjust = 0), axis.title = element_text(size = 0,
                                                                                  hjust = 0), legend.text = element_text(size = 0, hjust = 0),
                                       legend.title = element_text(size = 0, hjust = 0) )
################################################################
## inline function
inline = function (x) {  shiny::tags$div(style="display:inline-block;", x)  }
################################################################
thanos =
    'shinyThanos is a tool to allow filtering of data frames. - -Nathan Siemers'
################################################################
ui = fluidPage(
    titlePanel( 'shinyThanos - a data interrogation and filtering toolset' ),
    sidebarLayout(
        sidebarPanel(
            actionButton('go', 'Plot'),
            tags$br(), tags$br(),
            ################################################################
            ## needed for thanos
            selectizeInput('selectthings', 'select things',
                           choices = NULL, multiple = TRUE),
            uiOutput('thanos'),
            ################################################################
            tags$br(), tags$br(),
            verbatimTextOutput('print2'),
            verbatimTextOutput('print1')
            ),
        ### this stuff below is just to draw an example graph, not part of thanos
        mainPanel(
            div(withSpinner(plotOutput( 'plotter', width = '100%', height = 700 ))),
            splitLayout(
                selectizeInput('plotthings', 'graph things',
                               choices = NULL, multiple = TRUE),
                selectizeInput('colorthings', 'color by',
                               choices = NULL),
                selectizeInput('facetthings', 'graph for each', 
                               choices = NULL),
                tags$head(tags$style(HTML("
                 .shiny-split-layout > div {
                 overflow: visible;
                 }" )))
                ),
            tags$br(), tags$br(),
            tags$br(), tags$br(),
            tags$br(), tags$br(),
            tags$br(), tags$br(),
            tags$br(), tags$br(),
            tags$br(), tags$br(),
            tags$br(), tags$br()
            )
        )
    )
    
################################################################
server = function( input, output, session ) {
    print('server')
    updateSelectizeInput(
                 session, 'selectthings',  choices = colnames(data),
                 selected = data_selected, server = TRUE )
    updateSelectizeInput(session, 'plotthings',  choices = colnames(data),
                         selected = data_selected[2:3], server = TRUE )
    updateSelectizeInput(session, 'colorthings',  choices = colnames(data),
                         selected = data_selected[1], server = TRUE )
    updateSelectizeInput(session, 'facetthings',  choices = data_categorical,
                         selected = data_facet, server = TRUE )
    getData = function( mydata = data, myby = input$selectthings) {
        for( i in myby ) {
            if(
                ##(! is.numeric( mydata[, i] ) )
                (! is.numeric( mydata[, i] ) )  |
                length(unique( mydata[, i] ) ) <=  unique_categorical 
                ) {
                if(!is.null( input[[i]] ) ) {
                    mydata = mydata[ mydata[, i] %in% input[[i]], ]
                }
            }
            else {
                if(!is.null( input[[i]] ) ) {
                    mydata = mydata[ mydata[, i] >= input[[i]][[1]], ]
                    mydata = mydata[ mydata[, i] <= input[[i]][[2]], ]
                }
            }
        }
        mydata
    }
    ## observe({
    ##     for( i in input$selectthings ) {
    ##         input[[ paste0('selectall_', i) ]]
    ##         choices = sort(unique(data[,i]))
    ##         updateCheckboxGroupInput(session, i, i, choices= choices, selected = isolate(choices), inline = TRUE)
    ##     }
    ## })
    observe({
        Map(function(name) {
            print("Map")
            print(name)
            if( is.null( input[[name]] ) )  return(NULL)
            selectthings = input$selectthings
            print(paste('selectthings:', selectthings))
            if(
                is.numeric( data[, name] )  &
                length(unique( data[, name] ) ) > unique_categorical 
                ) {
                print('we are here')
                minvar = min(data[,name], na.rm = TRUE)
                maxvar  = max(data[,name], na.rm = TRUE)
                mostthings = selectthings
                mostthings = mostthings[ mostthings != name ]
                data = getData(data, mostthings)
                if ( length(input[[name]]) == 2 ) {
                    one = input[[name]][[1]]; two = input[[name]][[2]]
                    filtering = data[,name] >= one & data[,name] <=  two
                    filtering = factor(ifelse( filtering, 'sel', 'unsel'), levels = c('sel', 'unsel') )
                    output[[paste0('hist_', name)]] <-  renderPlot({
                        p = ggplot( data ) +
                            geom_histogram( bins = 60, aes_string(
                                                x = name,
                                                fill = filtering
                                ) ) + theme_hist + scale_fill_gdocs(drop = FALSE) +
                                    xlim(minvar, maxvar)
                        p
                    }) 
                }
            } else {
                print("entering categorical histogram")
                mostthings = input$selectthings
                mostthings = mostthings[ mostthings != name ]
                data = getData(data, mostthings)
                if( length(which( data[, name] %in% input[[ name]])) == length(data[,name])) {}
                filtering = data[,name] %in% input[[ name ]]
                filtering = factor(ifelse( filtering, 'sel', 'unsel'), levels = c('sel', 'unsel') )
                print(head(filtering))
                output[[paste0('hist_', name)]] <-  renderPlot({
                    p = ggplot( data ) +
                        geom_histogram( stat="count", aes_string(
                                            x = name,
                                            fill = filtering
                            ) ) + theme_hist + scale_fill_gdocs(drop = FALSE) 
                    p
                })
            }
        },
            input$selectthings )
    })
    p = eventReactive( input$go, { 
        if ( is.null(input$plotthings) ) return(NULL)
        if ( input$plotthings == "" ) return(NULL) 
        if ( length(input$plotthings) < 2 ) return(NULL) 
        for(i in input$selectthings) {
            input[[i]]
        }
        mydata = getData(data)
        colorthings = input$colorthings
        if ( colorthings == "") colorthings = NULL
        p = ggplot(mydata) +
            geom_point( aes_string(
                x = input$plotthings[[1]],
                y = input$plotthings[[2]],
                color = colorthings ) )
        if (!  input$facetthings == "" ) {
            my.formula = as.formula( paste( '~', input$facetthings ) ) 
            p = p + facet_wrap( my.formula )
        }
        p = p + ggtitle('Thanos filtering tool',
                    subtitle =
                        paste( nrow(mydata), 'of', nrow(data), 'rows. Plot:',
                              input$plotthings[[1]], 'vs.', input$plotthings[[2]] )
            )
        p
    })
    output$plotter = renderPlot({ p() }  )
    output$thanos = renderUI({
        print('enter outpu$thanos')
        out= tagList()
        mydata = getData(data)
        for ( i in input$selectthings )   {
            things_in_i = unique( mydata[, i] )
            if( is.numeric( data[, i] ) &
               length(unique(data[, i])) >= unique_categorical  ) {
                minvar = min(data[,i], na.rm = TRUE)
                maxvar  = max(data[,i], na.rm = TRUE)
                step = signif( ( maxvar - minvar ) / 100, digits = 1 )
                minsig = minvar - step
                maxsig = maxvar + step
                if( is.null( input[[i]] ) ) {
                    thisslider = c(minsig, maxsig)
                } else {
                    thisslider = input[[i]]
                }
                print(thisslider)
                print(minvar)
                print(maxvar)
                out = tagAppendChild(out,
                    div(
                        useShinyCustom(
                            slider_delay = "2000",
                            numeric_delay = "2000",
                            text_delay = "2000"
                            ), 
                        customSliderInput(i, i, min = minvar, max = maxvar,
                                          value = c(thisslider),
                                          width = 300 )
                        ))
                out = tagAppendChild(out,
                    plotOutput( paste0('hist_', i), width = 300, height = 100) )
            } else {
                varfun = debounce(reactive( input[[i]] ), millis = 1500)
                if(is.null(varfun())) print( 'varfun is null')
                print(head(paste( 'varfun categorical is:', varfun() )))
                if( is.null(varfun() )) {
                    myvar = sort(unique( mydata[ , i] ))
                } else {
                    myvar = varfun()
                }
                out = tagAppendChild ( out,
                    div(
                        checkboxGroupInput( i, i,
                                           choices = sort(unique( data[, i] )),
                                           ##selected = myvar[ myvar %in% things_in_i ],
                                           selected = myvar,
                                           inline = TRUE
                                           )
                        ##actionButton(
                        ##    paste0('selectall_', i), 'All' )
                        ##)
                                      ) )
                if ( length(unique(data[ , i] )) <= max_hist_cat ) {
                    print( "less than max_hist_cat")
                    print(max_hist_cat)
                    out = tagAppendChild(out,
                        plotOutput( paste0('hist_', i), width = 300, height = 100) )
                }
            }
        }
        out
    })
    output$print1 = renderPrint({
        print( str( reactiveValuesToList(input) ) ) } )
    output$print2 = renderPrint({
        print("Original Data")
        print(dim(data))
        print("Thanos Result")
        print( dim( getData() ) )
        print( data_description )
        print("Fields available for search (might be partial)")
        print(head(colnames(data), 40))
        print("Fields available for search (might be partial)")
        print(head(str(data), 40))
    })
}

shinyApp(ui, server)
