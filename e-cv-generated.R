# e-cv-generated.R
# main program to produce file e-cv-generated.makefile 
# the file e-cv-generated.makefile contains rules and recipes to build every possible output
# from e-cv.R

# upon execution of make -f e-cv-generated.makefile, files of with these names are generated
# WORKING/e-cv_global_linear_2009_SCENARIO_RESPONSE_always_PREDICTORSFORM_NDAYS_0_0_0.RData
#
# Command line arguments: None
#
# TODO:
# generate multiple targets to allow resulting script to be started on multiple systems via
# (on system 1) : make -f e-cv-generate.makefile targets.1.of.6 -j 7
# (on system 2) : make -f e-cv-generate.makefile targets.2.of.6 -j 7

source('Directory.R')
source('Libraries.R')

source('CrossValidateCharts.R')
source('CVApplyAllPossibilities.R')
source('CvPossible.R')

library(ggplot2)
library(optparse)

Control <- function(default.args) {
    # parse command line arguments in command.args
#    opt <- ParseCommandArgs( command.args = commandArgs(trailingOnly = TRUE)
#                            ,default.args
#                            )

    me <- 'e-cv-generated' 

    log <- Directory('log')
    working <- Directory('working')
    cells <- paste0(working, 'e-cv-cells/')

    control <- list( path.out.log = paste0(log, me, '.log')
                    ,path.out = paste0(me, '.makefile')  # write to src directory
                    ,dir.cells = cells
                    ,testing = FALSE
                    ,debug = FALSE
                    ,possible = CvPossible()
                    ,working = working
                    ,me = me
                    )
    control
}
#ParseCommandArgs <- function(command.args, default.args) {
#    # return name list of values from the command args
#    opt.query <- make_option( opt_str = c('--query')
#                             ,action = 'store'
#                             ,type = 'double'
#                             ,default = default.args$query
#                             ,help = 'fraction of samples used as queries'
#                             )
#    option.list <- list( opt.query
#                        )
#    opt <- parse_args( object = OptionParser(option_list = option.list)
#                      ,args = command.args
#                      ,positional_arguments = FALSE
#                      )
#    opt
#}
Lines <- function(max.size = 1000) {
    # class object: list of lines
    lines <- rep('', max.size)
    last.index <- 0

    Append <- function(line) {
        last.index <<- last.index + 1
        lines[[last.index]] <<- line
    }

    Get <- function() {
        lines[1:last.index]
    }

    list( Append = Append
         ,Get = Get
         )
}
FileNamesCommands <- function(working) {
    # class object: list of filenames and commands
    # ARG working: path to working directory in file system
    file.names <- Lines()
    commands <- Lines()

    Accumulate <- function(scope, model, timePeriod, scenario, response
                           ,predictorsName, predictorsForm, ndays
                           ,query, c, ntree, mtry) {
        # accumulate file names and command for interesting scope, model, timePeriod, predictorsName
        # mutate state
        # return nothing
            
        file.name <- paste0( working 
                            ,'e-cv-cells/', scope
                            ,'_', model
                            ,'_', timePeriod
                            ,'_', scenario
                            ,'_', response
                            ,'_', predictorsName
                            ,'_', predictorsForm
                            ,'_', ndays
                            ,'_', query
                            ,'_', c
                            ,'_', ntree
                            ,'_', mtry
                            ,'.RData'
                            )
        file.names$Append(file.name)
        command <- paste( 'Rscript'
                         ,'e-cv.R'
                         ,'--scope', scope
                         ,'--model', model
                         ,'--timePeriod', timePeriod
                         ,'--scenario', scenario
                         ,'--response', response
                         ,'--predictorsName', predictorsName
                         ,'--predictorsForm', predictorsForm
                         ,'--ndays', ndays
                         ,'--query', '1'
                         ,'--c', '0'
                         ,'--ntree', '0'
                         ,'--mtry', '0'
                         )
        commands$Append(command)
    }

    Get <- function() {
        list( file.names = file.names$Get()
             ,commands = commands$Get()
             )
    }

    list( Accumulate = Accumulate
         ,Get = Get
         )
}
DetermineFileNamesAndCommands <- function(control) {
    # return list $file.names $commands
    file.names.commands <- FileNamesCommands(control$working)
    CvApplyAllPossibilities( FUN = file.names.commands$Accumulate
                            ,possible = control$possible
                            )
    fnc <- file.names.commands$Get()
    fnc
}
GenerateMakefile <- function(file.names, commands, control) { 
  browser()
    # general the make file lines
    lines <- Lines()
#    GenerateTargets <- function(file.names) {
#        # accumulate targets definitions into lines
#        for (file.name in file.names) {
#            line <- paste( 'targets +='
#                          ,file.name
#                          )
#            lines$Append(line)
#        }
#    }
#
#    GenerateTargets(file.names)
#
#    lines$Append('.PHONY: all')
#    lines$Append('all: $(targets)')

    lines$Append('# this makefile generated by program e-cv-generated.R')
    lines$Append(sprintf('# e-cv-generated.R was run at %s', Sys.time()))
    lines$Append('#')

    GenerateRulesAndRecipes <- function(file.names, commands) {
        # accumulate rules into lines
        stopifnot(length(file.names) == length(commands))
        for (index in 1:length(file.names)) {
            file.name <- file.names[[index]]
            command <- commands[[index]]
            rule <- paste(file.name, ':', 'e-cv.R') 
            recipe <- paste('\t', command)
            lines$Append(rule)
            lines$Append(recipe)
            if (control$testing) break
        }
    }

    GenerateRulesAndRecipes(file.names, commands)
    result <- lines$Get()
    result
}
Main <- function(control) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')

    fnc <- DetermineFileNamesAndCommands(control)
    file.names <- fnc$file.names
    commands <- fnc$commands

    makefile.lines <- GenerateMakefile(file.names, commands, control)

    # write the file
    writeLines( text = makefile.lines
               ,con = control$path.out
               )
}



### Execution starts here

clock <- Clock()

default.args <- NULL  # there are no command line args

control <- Control(default.args)

Main(control)
Printf('took %f CPU minutes\n', clock$Cpu()  / 60)
Printf('took %f wall clock minutes\n', clock$Wallclock() / 60)
if (control$testing)
    cat('DISCARD RESULTS: TESTING')
cat('done\n')
