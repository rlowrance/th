# e-cv-generated.R
# main program to produce file e-cv-generated.makefile 
# this makefile has rules for every file in WORKING/e-cv-cells/ that is
# actually used (Hence not all files)
# 
# Usage:
# - include in a bigger makefile
# - make -f e-cv-generated.makefile -j 8 part1
#   to generate all files in part1 using 8 CPUs
# - make -f e-cv-generated.makefile -j 8 part3
#   to generate all files in part2 using 8 CPUs
# - make -f e-cv-generated.makefile -j 8 parts-all
#   to generate all the files actuall needed
#
# the rules are of the form;
# WORKING/e-cv-cells/SCOPE_MODEL_TIMEPERIOD ... .RData:
#   Rscript e-cv.R --scope SCOPE --model MODEL --timePeriod ...

# Command line arguments: None
#

source('Directory.R')
source('Libraries.R')

source('CrossValidateCharts.R')
source('CVApplyAllPossibilities.R')
source('CvCell.R')
source('CvPossible.R')
source('Lines.R')

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
DetermineFileNamesAndCommands <- function(control) {
    # return list $file.names $commands
    Path <- CvCell()$Path
    Command <- CvCell()$Command
  

    file.names <- Lines()
    commands <- Lines()

    # append entries for linear models
    for (scope in 'global') {
      for (model in 'linear') {
        for (timePeriod in c('2003on', '2008')) {
          for (scenario in 'avm') {
            for (response in c('price', 'logprice')) {
              for (predictorsForm in c('level', 'log')) {
                for (predictorsName in c( 'always'
                                         ,'alwaysNoAssessment'
                                         ,'alwaysNoCensus'
                                         )) {
                  for (ndays in c( '30', '60', '90', '120', '150',
                                  '180', '210', '240', '270', '300',
                                  '330', '360')) {
                    query <- if (timePeriod == '2003on') '100' else '1'
                    file.name <- Path( scope = scope
                                      ,model = model
                                      ,timePeriod = timePeriod
                                      ,scenario = scenario
                                      ,response = response
                                      ,predictorsForm = predictorsForm
                                      ,predictorsName = predictorsName
                                      ,ndays = ndays
                                      ,query = query
                                      ,c = '0'
                                      ,ntree = '0'
                                      ,mtry = '0'
                                      )
                    file.names$Append(file.name)
                    command <- Command( scope = scope
                                       ,model = model
                                       ,timePeriod = timePeriod
                                       ,scenario = scenario
                                       ,response = response
                                       ,predictorsForm = predictorsForm
                                       ,predictorsName = predictorsName
                                       ,ndays = ndays
                                       ,query = query
                                       ,c = '0'
                                       ,ntree = '0'
                                       ,mtry = '0'
                                       )
                    commands$Append(command)
                  }
                }
              }
            }
          }
        }
      }
    }

    # append more entries for the best features analysis 
    # and for the pca features analysis
    best.predictors.names <- sapply(1:24, function(n) sprintf('best%02d', n))
    pca.predictors.names <- sapply(1:4, function(n) sprintf('pca%02d', n))
    print(best.predictors.names)
    print(pca.predictors.names)
    for (predictorsName in c(best.predictors.names, pca.predictors.names)) {
      file.name <- Path( scope = 'global'
                        ,model = 'linear'
                        ,timePeriod = '2003on'
                        ,scenario = 'avm'
                        ,response = 'logprice'
                        ,predictorsForm = 'level'
                        ,predictorsName = predictorsName
                        ,ndays = '60'
                        ,query = '100'
                        ,c = '0'
                        ,ntree = '0'
                        ,mtry = '0'
                        )
      file.names$Append(file.name)
      command <- Command( scope = 'global'
                         ,model = 'linear'
                         ,timePeriod = '2003on'
                         ,scenario = 'avm'
                         ,response = 'logprice'
                         ,predictorsForm = 'level'
                         ,predictorsName = predictorsName
                         ,ndays = '60'
                         ,query = '100'
                         ,c = '0'
                         ,ntree = '0'
                         ,mtry = '0'
                         )
      commands$Append(command)
    }

    # append more entries for the regularized linear regression analysis
    possible.c.values <- CvCell()$Possible.C.Values()
    for (c.value in possible.c.values) {
      file.name <- Path( scope = 'global'
                        ,model = 'linL2'
                        ,timePeriod = '2003on'
                        ,scenario = 'avm'
                        ,response = 'logprice'
                        ,predictorsForm = 'level'
                        ,predictorsName = 'best20'
                        ,ndays = '60'
                        ,query = '100'
                        ,c = c.value
                        ,ntree = '0'
                        ,mtry = '0'
                        )
      file.names$Append(file.name)
      command <- Command( scope = 'global'
                         ,model = 'linL2'
                         ,timePeriod = '2003on'
                         ,scenario = 'avm'
                         ,response = 'logprice'
                         ,predictorsForm = 'level'
                         ,predictorsName = 'best20'
                         ,ndays = '60'
                         ,query = '100'
                         ,c = c.value
                         ,ntree = '0'
                         ,mtry = '0'
                         )
      commands$Append(command)
    }

    result <- list( file.names = file.names$Get()
                   ,commands = commands$Get()
                   )
    result
}
GenerateMakefile <- function(file.names, commands, control) { 
    # general the make file lines
    lines <- Lines()

    lines$Append('# this makefile generated by program e-cv-generated.R')
    lines$Append(sprintf('# e-cv-generated.R was run at %s', Sys.time()))
    lines$Append('#')

    GenerateRulesAndRecipes <- function(file.names, commands, lines) {
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

    GenerateAll1All2 <- function(file.names, lines) {
      # all1 += file.name[[1]]
      # all2 += file.name[[2]]
      # all1 += file.name[[3]]
      # all2 += file.name[[4]]
      # ...
      # .PHONY: all
      # all: all1 all2
      # .PHONY: all1
      # all1: $(all1)
      # .PHONY: all2
      # all2: $(all2)

      n <- 0
      for (file.name in file.names) {
        n <- if (n == 2) 1 else (n + 1)
        line <- sprintf('all%d += %s', n, file.name)
        lines$Append(line)
      }

      lines$Append('.PHONY: all')
      lines$Append('all1: all1 all2')

      lines$Append('.PHONY: all1')
      lines$Append('all1: $(all1)')

      lines$Append('.PHONY: all2')
      lines$Append('all2: $(all2)')
    }

    GenerateRulesAndRecipes(file.names, commands, lines)
    #GenerateAll1All2(file.names, lines)

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
    Printf('created rules for %d files\n', length(file.names))

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
