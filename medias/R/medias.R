# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

transTon <- function(field, id_name){#transform id to name
  refiel <- str_replace_na(field)
  ln = nrow(id_name)

  for(i in 1:ln){
    id = id_name[[1]][i]
    ide = str_c('(?<=\\|)', id, '(?=\\|)')

    refiel = str_replace_all(refiel, ide, id_name[[2]][i])
  }

  return(refiel)
}


wipeValid <- function(field, tags){
  refiel <- str_replace_na(field)
  tlen <- length(tags)

  for(i in 1:tlen){
    tg = str_c('\\|', tags[i], '\\|')
    refiel = str_replace_all(refiel, tg, '|')
  }

  refiel = str_replace_all(refiel, '(?<![0-9])\\|(?![0-9])', '')

  return(refiel)
}

wipeValid_f <- function(fields, tags, fgenr, tgenr, rgenr){##fields and tags all have attribute fstlvlname
  #fgenr, genre of fields need to wipe off;
  #tgenr, genre of tags to wipe fields
  #rgenr, restrict genre
  rg = tags[[rgenr]] %>% unique
  fields[[fgenr]] = str_replace_na(fields[[fgenr]])

  for(r in rg){
    utg <- tags %>% filter(.data[[rgenr]] == r) %>% `[[`(tgenr)
    fields <- fields %>% mutate(.data[[fgenr]] = ifelse(.data[[rgenr]] == r,
                                                        wipeValid()))
  }
}
