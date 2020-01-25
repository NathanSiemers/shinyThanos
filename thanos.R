################################################################
## functions needed for thanos
################################################################
## this code plus a map creates all the dynamic filters
library(viridis)

theme_thanos = theme(axis.text.x = element_text(size = 12),  ####, angle = 30, hjust = 10)
    axis.text.y = element_text(size = 9),
    legend.position = "none",
    plot.title = element_text(size = 12, hjust = 0),
    axis.title = element_text(size = 0, hjust = 0),
    legend.text = element_text(size = 0, hjust = 0),
    legend.title = element_text(size = 0, hjust = 0) )

    
ui_filters = function(x, selectedvals, var, picket, width = '100%', height = '200px') {
    ## these pickets are all just my duplications of terms in the map()
    ## so I can do multiple things for each ui search category
    ## make a slider/checkbox, than an all/none checkbox, then print a histogram, etc
    if( picket == 1) {
    } else if (picket == 2) {
        if (is.numeric(x)) {
            rng <- range(x, na.rm = TRUE)
            ##print(paste('range is:', rng))
            if( length(selectedvals) == 2 ) { ## we have values
                sliderInput(var, var, min = rng[1], max = rng[2], value = selectedvals, width = width)
            } else {
                sliderInput(var, var, min = rng[1], max = rng[2], value = rng, width = width)
            }
        } else {
            levs <- sort(unique((x)))
            ##print(paste( "within checkbox creation: selectedvals is:", selectedvals ))
            action = 1
            ##if ( is.null(selectedvals) & FALSE ) {
            if ( length(selectedvals) < 1 ) {
                checkboxGroupInput(var, var, choices = levs, selected = levs, inline = TRUE, width = width) 
            } else {
                checkboxGroupInput(var, var, choices = levs, selected = selectedvals, inline = TRUE, width = width)
            }
        }
    } else if (picket == 3 ) {
        if( !is.numeric(x) ) {
            alink = paste0('action_', var)
            actionLink( alink, 'all/none')
        }
    } else if (picket == 4 ) {
        plot_obj = paste0('plot_', var)
        ##div(style = "height:100px;", plotOutput(plot_obj, height = 100,, width = width) )
        plotOutput(plot_obj, height = height, width = width)
    }  else if (picket == 5) {
    }
}


filter_var <- function(data_list, var, val) {
    ## print('in filter_var')
    ## print(paste('variable is', var))
    ## print(paste('value is', str(val)))
    l = lapply(data_list, function (dat)  {
        ## print('head of dat')
        ## print(head(dat))
        ## print(colnames(dat))
        if ( var %in% colnames(dat)) {
            x = dat[ , var]
            if (is.numeric(x)) {
                out = is.na(x) | x >= val[1] & x <= val[2]
            } else {
                out = is.na(x) | x %in% val
            }
        } else {
            ## print(paste('did not find',  var, 'in this df'))
            out = rep(TRUE, nrow(dat) )
        }
        ## print('filter_var lapply')
        ## print(table(out))
        out
    })
    Reduce("&", l)
}

get_variable_in_data_list = function(dl, var) {
    out = lapply( dl, function(x) {
        if ( var %in% colnames(x) ) x[ , var]
    })
    out[vapply(out, Negate(is.null), NA)][[1]]
}

get_cols_in_data_list = function(dl) {
    unique(unlist(sapply(dl, colnames)))
}

plot_it = function(data_list, selected, x){
    var = get_variable_in_data_list( data_list, x )
    ## print("in plot_it")
    ## print(paste("name is:", x))
    ## print(paste('var length:', length(var)))
    ## print(str(var))
    ## print('selected:')
    ## print(str(selected))
    filter_most = selected[ names(selected) != x  ]
    ## print("filter_most")
    ## print(str(filter_most))
    reduced = Reduce( "&", filter_most )
    ## print('structure of reduced')
    ## print(str(reduced))
    filtered_most = var[ reduced ]
    filter_last =  selected[[ x ]]
    ## print("filter_last")
    ## print(str(filter_last))
    filter_last = filter_last[ reduced ] 
    ## print(length(filter_last))
    ## print(str(filter_last))
    plot_name = paste0('plot_', x)
    ## print('ready to plot')
    ## print('x')
    ## print(head(filtered_most))
    ## print('fill')
    ## print(head(filter_last))
    fcolor = factor(ifelse( filter_last, 'sel', 'unsel'), levels = c('sel', 'unsel') )
    plot_title = paste( x, ':', length(which(filter_last)),  '/', length(filtered_most) )
    if (is.numeric(var)) {
        p = ggplot() + geom_histogram( aes( x = filtered_most, fill = fcolor ), bins = 50 )  +
            ggtitle( plot_title ) +
            scale_fill_viridis( end = 0.4, discrete = TRUE, option = "plasma" )
    } else {
        p = ggplot() + geom_histogram( aes( x = filtered_most, fill = fcolor), stat = "count" ) +
            ggtitle( plot_title ) +
                scale_x_discrete(labels = abbreviate) +
            scale_fill_viridis( end = 0.4, discrete = TRUE, option = "plasma" )
    }
    p + theme_thanos
}

## try to get thanos into function calls/libs
thanos_selectize = function( input, output, session, data_list, default_selected = c() ) {
    ## print('inside thanos_select')
    updateSelectizeInput(
        session, 'selectthings',  choices = get_cols_in_data_list(data_list),
        selected = default_selected, server = TRUE )
}
thanos_selected = function( input, output, session, data_list) {
    ##print('in thanos selected')
    reactive({
        ##req( input$selectthings )
        ##map(input$selectthings, ~ print( input[[.x]] ) )
        each_var <- map(input$selectthings, ~ filter_var(data_list = data_list, .x, input[[.x]]) )
        ##print('thanos_selected each_var')
        ##print(head(each_var))
        ##
        ##reduce(each_var, `&`)
        names(each_var) = input$selectthings
        each_var
    })
}

thanos_histos = function( input, output, session, data_list, selected) {
##    observe({
    ##        map( input$selectthings, ~ input[[.x]] )
    ## print("inside thanos_histos")
    cname = paste0('plot_', input$selectthings)
    Map(function(name) {
        cname = paste0('plot_', name)
        output[[cname]] <<- renderPlot({
            plot_it( data_list, selected, name )
        }) ##, res = 300 )
    },
        input$selectthings )
    ## print('exit thanos_histos')
    ##    }, priority = 5 )
    ##    output
}

thanos_allnone = function(input, output, session, data_list, x) {
    act = input[[ paste0('action_', x) ]]
    ## print('love/hate')
    ## print(x)
    allchoices = sort(unique(get_variable_in_data_list( data_list, x ) ) )
    if ( as.numeric(act) %% 2 == 0 ) {
        return (updateCheckboxGroupInput( session, x, x, choices = allchoices, selected = allchoices, inline = TRUE ) )
    } else {
        return (updateCheckboxGroupInput( session, x, x, choices = allchoices, selected = character(0), inline = TRUE ))
    }
}

## this isn't used (yet!)
thanos_renderfilters = function(input, output, session, data_list, selectthings, selected, dups = 5) {
    ## print('inside thanos_renderfilters')
    output$renderfilters <<- renderUI({
        ## print('begin rep renderfilters')
        dupnames = unlist(lapply(input$selectthings,  function(x){ rep(x, dups) } ) )
        ## print('end rep renderfilters')
        dupcounts = rep(1:dups, length( input$selectthings ) )
        ##tagList(map2(dupnames, dupcounts, ~ ui_filters( data[[.x]], isolate(input[[.x]]), .x, data[selected, ], .y ) ))
        tagList(map2(dupnames, dupcounts, ~ ui_filters( get_variable_in_data_list( data_list, .x ), isolate(input[[.x]]), .x, .y ) ))
    })
}

################################################################
## end of functions needed for thanos
################################################################
