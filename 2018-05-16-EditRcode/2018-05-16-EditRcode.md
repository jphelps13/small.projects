This is a simplified version of a script I wrote for work. We have a
large code base, built up over many years. The end result was a mismatch
in formatting.

This script is an example of bulk editing R scripts using regular
expressions. In particular, standardising the naming convention of R
functions to the piped format eg `thisIsPiped <- function(){...}`.

Please have these packages:

    # packages
    library(magrittr)
    library(data.table)
    library(rprojroot) # I'm using an rstudio project in my root folder

For this tutorial, I have saved in to a "/scripts" folder two files:

**functions.R** - script of functions with different naming conventions

    fun_clean <- function(x){x}
    fun_visualise_data <- function(x){x}
    Fun.Model <- function(x){x}
    funReport <- function(x){x}

**run.R** - script that runs the functions

    x <- 1
    fun_clean(x)
    fun_visualise_data(x)
    Fun.Model(x)
    funReport(x)

We are going to change them all to the piped structure. The regex I am
using can be found **[Here](https://regex101.com/r/dUBRzV/5)**. The
breakdown of the regex is as follows. I'll be using the example
`fun_visualise_data <- function(x)`.

`^([^#\n]*)`: The first group captures all lines that don't start with a
\# ie are comment lines in R. The `\n` isn't needed here if we are
evaluating each line of the R script one at a time. It is only useful if
running the regex over a block of lines and functions. e.g. returns
"**fun\_visualise**"

`([_\.])`: The second group looks for a single `.` and `_` character.
This will match the last one found in e.g. returns
"fun\_visualise**\_**"

`([^\s,=]*)`: The third group makes the assumption that the function
name won't end in the `.` or `_`, and will grab the final text entry.
e.g. returns "fun\_visualise\_**data**"

`(\s*)`: The fourth group catches any space text between the function
name and its assignment.

`(?:(?:<-|=)\s*):` The fifth group (non-capturing) will pick out the
assignment variable and any space between this and the function call.
e.g. returns "fun\_visualise\_data **&lt;-**"

`(function)\\(`: finally only interested in variables that are
functions. Full string is now captured

Ok, so first I will use the function `readLines` to read in the scripts
in to a list `read_files`. I've unlisted these so that I have one single
lookup too.

    # script path - assume you start at the root. replace root_path with your path
    # if you aren't using an Rstudio project. rmd is in root, files are in ./scripts.
    root_path <- rprojroot::find_rstudio_root_file()
    script_path <- file.path(root_path, "scripts")
    setwd(script_path)
    all_files <- c("run.R", "functions.R")

    # read in the text to R
    read_files <- lapply(all_files, function(file){
      readLines(file)
    })
    names(read_files) <- all_files
    unlist_files <- unlist(read_files)

This code takes the functions script, finds all that matches the
pattern, and returns only the parts before the assignment variable. I
did this with base at the time, but I'd recommend re-writting with the
`stringi` package for practice.

Special character need to be escaped with an extra backslash. See this
website for getting started with Regex in R:
<https://www.regular-expressions.info/rlanguage.html>

    # run through functions
    script <- "functions.R"
    # search and replace to get the functions that need piping
    f <- read_files[[script]]
    pattern <- "^([^#\\n]*)([_\\.])([^\\s,=]*)(\\s*)(?:(?:<-|=)\\s*)(function)\\("
    sub     <- "\\1\\2\\3"
    matches <- regmatches(f, regexpr(pattern, f, perl = TRUE))
    extract <- gsub(pattern, sub, matches, perl = TRUE) %>% trimws()
    matches

    ## [1] "fun_clean <- function("          "fun_visualise_data <- function("
    ## [3] "Fun.Model <- function("

    extract

    ## [1] "fun_clean"          "fun_visualise_data" "Fun.Model"

Pipe them:

    orig    <- "(?:[_\\.])(.)"
    piped   <-  "\\U\\1"
    extract_v2 <- gsub(orig, piped, extract, perl = TRUE)
    extract_v2

    ## [1] "funClean"         "funVisualiseData" "FunModel"

lowercase first letter:

    orig    <- "^(.)"
    lowered <- "\\L\\1"
    extract_v2 <- gsub(orig, lowered, extract_v2, perl = TRUE)
    extract_v2

    ## [1] "funClean"         "funVisualiseData" "funModel"

Store the details of the changes:

    change_dt <- data.table(script = script, orig = extract, change = extract_v2)
    N <- nrow(change_dt)
    change_dt[]

    ##         script               orig           change
    ## 1: functions.R          fun_clean         funClean
    ## 2: functions.R fun_visualise_data funVisualiseData
    ## 3: functions.R          Fun.Model         funModel

Once we have the changes, simply loop across the scripts and change all
cases. Made use of `\\b` to define boundaries so that e.g. `fun_clean_2`
would not be matched when looking for `fun_clean`.

    # now loop and replace in the files
    alter_files <- lapply(seq_along(read_files), function(i){
      out <- read_files[[i]]
      for(j in 1:N){
        f <- change_dt[j, ]
        out <- gsub(sprintf("\\b%s\\b", f$orig),
                    f$change,
                    out, 
                    perl = TRUE)
      }
      out
    })
    names(alter_files) <- names(read_files)

Lets have a look at them both:

    alter_files[["functions.R"]]

    ## [1] "# sample file with functions in"   
    ## [2] "funClean <- function(x){x}"        
    ## [3] "funVisualiseData <- function(x){x}"
    ## [4] "funModel <- function(x){x}"        
    ## [5] "funReport <- function(x){x}"

    alter_files[["run.R"]]

    ## [1] "# sample script"     "x <- 1"              "funClean(x)"        
    ## [4] "funVisualiseData(x)" "funModel(x)"         "funReport(x)"

Perfect!

Finally, save them out with a new suffix, to check. The original files
can be over-written when you're happy.

    for(i in seq_along(alter_files)){
      file <- names(alter_files)[[i]]
      file_new <- gsub("\\.(?:[rR])", "_updated.R", file)
      cat(alter_files[[i]], file = file_new, sep="\n")
    }

I am not a regex expert. If you have any suggestions on how to make this
more efficient, please drop me a message :) Thank you
