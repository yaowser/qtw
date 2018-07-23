#### R Source File for Prepping Data for Analysis ####
#Yao, Kruse, Angelov

ubase = "http://www.cherryblossom.org/"

menURLs = 
  c("results/1999/cb99m.html", "results/2000/Cb003m.htm", "results/2001/oof_m.html",
    "results/2002/oofm.htm", "results/2003/CB03-M.HTM",
    "results/2004/men.htm", "results/2005/CB05-M.htm", 
    "results/2006/men.htm", "results/2007/men.htm", 
    "results/2008/men.htm", "results/2009/09cucb-M.htm",
    "results/2010/2010cucb10m-m.htm", 
    "results/2011/2011cucb10m-m.htm",
    "results/2012/2012cucb10m-m.htm")

womenURLs = 
  c("results/1999/cb99f.html", "results/2000/Cb003f.htm", "results/2001/oof_f.html",
    "results/2002/ooff.htm", "results/2003/CB03-F.HTM",
    "results/2004/women.htm", "results/2005/CB05-F.htm", 
    "results/2006/women.htm", "results/2007/women.htm", 
    "results/2008/women.htm", "results/2009/09cucb-F.htm",
    "results/2010/2010cucb10m-f.htm", 
    "results/2011/2011cucb10m-f.htm",
    "results/2012/2012cucb10m-f.htm")

Murls = paste(ubase, menURLs, sep = "")
Wurls = paste(ubase, womenURLs, sep = "")

extractResTable =
  #
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 1999, sex = "male", file = NULL)
  {
    doc = htmlParse(url)
    
    if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
    else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }
    else if (year == 1999) {
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]]
    }
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
    } 
    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }

years = 1999:2012
menTables = mapply(extractResTable, url = Murls, year = years, sex = "male")
womenTables = mapply(extractResTable, url = Wurls, year = years, sex = "female")
names(menTables) = years
names(womenTables) = years
invisible(sapply(menTables, length))
invisible(sapply(womenTables, length))
save(menTables, file = "CBMenTextTables.rda")
save(womenTables, file = "CBWomenTextTables.rda")

womenTables$'2001'[2:3]<-womenTables$'2002'[2:3]

dir.create(file.path(getwd(), "MenTxt"))
dir.create(file.path(getwd(), "WomenTxt"))

write(x=menTables$'2012',file="MenTxt/2012.txt")
write(x=menTables$'2011',file="MenTxt/2011.txt")
write(x=menTables$'2010',file="MenTxt/2010.txt")
write(x=menTables$'2009',file="MenTxt/2009.txt")
write(x=menTables$'2008',file="MenTxt/2008.txt")
write(x=menTables$'2007',file="MenTxt/2007.txt")
write(x=menTables$'2006',file="MenTxt/2006.txt")
write(x=menTables$'2005',file="MenTxt/2005.txt")
write(x=menTables$'2004',file="MenTxt/2004.txt")
write(x=menTables$'2003',file="MenTxt/2003.txt")
write(x=menTables$'2002',file="MenTxt/2002.txt")
write(x=menTables$'2001',file="MenTxt/2001.txt")
write(x=menTables$'2000',file="MenTxt/2000.txt")
write(x=menTables$'1999',file="MenTxt/1999.txt")
write(x=womenTables$'2012',file="WomenTxt/2012.txt")
write(x=womenTables$'2011',file="WomenTxt/2011.txt")
write(x=womenTables$'2010',file="WomenTxt/2010.txt")
write(x=womenTables$'2009',file="WomenTxt/2009.txt")
write(x=womenTables$'2008',file="WomenTxt/2008.txt")
write(x=womenTables$'2007',file="WomenTxt/2007.txt")
write(x=womenTables$'2006',file="WomenTxt/2006.txt")
write(x=womenTables$'2005',file="WomenTxt/2005.txt")
write(x=womenTables$'2004',file="WomenTxt/2004.txt")
write(x=womenTables$'2003',file="WomenTxt/2003.txt")
write(x=womenTables$'2002',file="WomenTxt/2002.txt")
write(x=womenTables$'2001',file="WomenTxt/2001.txt")
write(x=womenTables$'2000',file="WomenTxt/2000.txt")
write(x=womenTables$'1999',file="WomenTxt/1999.txt")

# Review example race logs

els = readLines("WomenTxt/2012.txt")

# Identify line index for header-data break

eqIndex = grep("^===", els)
eqIndex

first3 = substr(els, 1, 3)
which(first3 == "===")

# Discard rows above header name line

spacerRow = els[eqIndex]
headerRow = els[eqIndex - 1]
body = els[ -(1:eqIndex) ]

# Extract runners' age

headerRow = tolower(headerRow)

ageStart = regexpr("ag", headerRow)
ageStart

age = substr(body, start = ageStart, stop = ageStart + 1)
head(age)

summary(as.numeric(age))

blankLocs = gregexpr(" ", spacerRow)
blankLocs

searchLocs = c(0, blankLocs[[1]])

Values = mapply(substr, list(body), 
                start = searchLocs[ -length(searchLocs)] + 1, 
                stop = searchLocs[ -1 ] - 1)

#Find locations of all blanks in line of '=' characters and extract columns
findColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}

# Extract columns

selectCols =  function(colNames, headerRow, searchLocs) 
  {
    sapply(colNames, 
           function(name, headerRow, searchLocs)
           {
             startPos = regexpr(name, headerRow)[[1]]
             if (startPos == -1) 
               return( c(NA, NA) )
             
             index = sum(startPos >= searchLocs)
             c(searchLocs[index] + 1, searchLocs[index + 1] - 1)
           },
           headerRow = headerRow, searchLocs = searchLocs )
  }

# Test findColLocs and selectCols functions

searchLocs = findColLocs(spacerRow)
ageLoc = selectCols("ag", headerRow, searchLocs) 
ages = mapply(substr, list(body), start = ageLoc[1,], stop = ageLoc[2, ])

summary(as.numeric(ages))

# Create shortened column identifiers and account for when some tables missing columns

shortColNames = c("name", "home", "ag", "gun", "net", "time")

locCols = selectCols(shortColNames, headerRow, searchLocs)

Values = mapply(substr, list(body), start = locCols[1, ], 
                stop = locCols[2, ])

class(Values)

colnames(Values) = shortColNames
head(Values)

tail(Values)[ , 1:3]

# Build wrapper function for column extraction

extractVariables = function(file, varNames =c("name", "home", "ag", "gun", "net", "time"))
  {
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    
    # Obtain the starting and ending positions of variables
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    invisible(Values)
  }

# Read table lines into R

mfilenames = paste("MenTxt/", 1999:2012, ".txt", sep = "")
menFiles = lapply(mfilenames, readLines)
names(menFiles) = 1999:2012
menFiles[['2009']] <- gsub("Â", "", menFiles[['2009']])

# Create list of character matrices containing the column contents for each of the 14 years of data

menResMat = lapply(menFiles, extractVariables)
sapply(menResMat, nrow)

# Read table lines into R

wfilenames = paste("WomenTxt/", 1999:2012, ".txt", sep = "")
womenFiles = lapply(wfilenames, readLines)
names(womenFiles) = 1999:2012
womenFiles[['2009']] <- gsub("Â", "", womenFiles[['2009']])

# Create list of character matrices containing the column contents for each of the 14 years of data

womenResMat = lapply(womenFiles, extractVariables)
sapply(womenResMat, nrow)

# Create numeric age variable

Mage = as.numeric(menResMat[['2012']][ , 'ag'])
Wage = as.numeric(womenResMat[['2012']][ , 'ag'])
tail(Mage)
tail(Wage)
Mage = sapply(menResMat, function(x) as.numeric(x[ , 'ag']))
Wage = sapply(womenResMat, function(x) as.numeric(x[ , 'ag']))

#pdf("CB_BoxplotAgeByYr.pdf", width = 8, height = 5)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

#boxplot(Mage, ylab = "Age", xlab = "Year")
#boxplot(Wage, ylab = "Age", xlab = "Year")

par(oldPar)
dev.off()

head(menFiles[['2003']])

menFiles[['2006']][2200:2205]

# Update selecCols to account for offset age values in age column

selectCols = function(shortColNames, headerRow, searchLocs) {
  sapply(shortColNames, function(shortName, headerRow, searchLocs){
    startPos = regexpr(shortName, headerRow)[[1]]
    if (startPos == -1) return( c(NA, NA) )
    index = sum(startPos >= searchLocs)
    c(searchLocs[index] + 1, searchLocs[index + 1])
  }, headerRow = headerRow, searchLocs = searchLocs )
}

menResMat = lapply(menFiles, extractVariables)
womenResMat = lapply(womenFiles, extractVariables)

Mage = sapply(menResMat, function(x) as.numeric(x[ , 'ag']))
Wage = sapply(womenResMat, function(x) as.numeric(x[ , 'ag']))

#pdf("CB_BoxplotAgeByYrRevised.pdf", width = 8, height = 5)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
boxplot(Mage, ylab = "Age", xlab = "Year")
title(main = "Male Runners' Age Per Year")
boxplot(Wage, ylab = "Age", xlab = "Year")
title(main = "Female Runners' Age Per Year")

par(oldPar)
dev.off()

sapply(Mage,  function(x) sum(is.na(x)))
sapply(Wage,  function(x) sum(is.na(x)))

Mage2001 = Mage[["2001"]]
Wage2001 = Wage[["2001"]]
grep("^===", menFiles[['2001']])
grep("^===", womenFiles[['2001']])
badAgeIndex = which(is.na(Mage2001)) + 5
badAgeIndex = which(is.na(Wage2001)) + 5

# Update extractVariables to account for missing age data

extractVariables = function(file, varNames =c("name", "home", "ag", "gun", "net", "time"))
  {
    
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data 
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    # Remove footnotes and blank rows
    footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
    if ( length(footnotes) > 0 ) body = body[ -footnotes ]
    blanks = grep("^[[:blank:]]*$", body)
    if (length(blanks) > 0 ) body = body[ -blanks ]
    
    
    # Obtain the starting and ending positions of variables   
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    return(Values)
  }

menResMat = lapply(menFiles, extractVariables)
womenResMat = lapply(womenFiles, extractVariables)

McharTime = menResMat[['2012']][, 'time']
WcharTime = womenResMat[['2012']][, 'time']

MtimePieces = strsplit(McharTime, ":")
WtimePieces = strsplit(WcharTime, ":")

MtimePieces = sapply(MtimePieces, as.numeric)
WtimePieces = sapply(WtimePieces, as.numeric)

MrunTime = sapply(MtimePieces, 
                  function(x) {
                    if (length(x) == 2) x[1] + x[2]/60
                    else 60*x[1] + x[2] + x[3]/60
                  })
WrunTime = sapply(WtimePieces, 
                  function(x) {
                    if (length(x) == 2) x[1] + x[2]/60
                    else 60*x[1] + x[2] + x[3]/60
                  })

# Split and process times

convertTime = function(time) {
  MtimePieces = strsplit(time, ":")
  MtimePieces = sapply(MtimePieces, as.numeric)
  sapply(MtimePieces, function(x) {
    if (length(x) == 2) x[1] + x[2]/60
    else 60*x[1] + x[2] + x[3]/60
  })
}

# Apply character matrices in menResMat and return a dataframe with variables

createDF = function(Res, year, sex) 
  {
    # Determine which time to use
    useTime = if( !is.na(Res[1, 'net']) )  
      Res[ , 'net']
    else if( !is.na(Res[1, 'gun']) ) 
      Res[ , 'gun']
    else 
      Res[ , 'time']
    
    runTime = convertTime(useTime)
    
    Results = data.frame(year = rep(year, nrow(Res)),
                         sex = rep(sex, nrow(Res)),
                         name = Res[ , 'name'],
                         home = Res[ , 'home'],
                         age = as.numeric(Res[, 'ag']), 
                         runTime = runTime,
                         stringsAsFactors = FALSE)
    invisible(Results)
  }

menDF = mapply(createDF, menResMat, year = 1999:2012,
               sex = rep("M", 14), SIMPLIFY = FALSE)

sapply(menDF, function(x) sum(is.na(x$MrunTime)))

createDF = function(Res, year, sex) 
{
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  Res = Res[ useTime != "", ]
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}

menDF = mapply(createDF, menResMat, year = 1999:2012,
               sex = rep("M", 14), SIMPLIFY = FALSE)

sapply(menDF, function(x) sum(is.na(x$runTime)))

# Fix missing runTime data issues for 2006

separatorIdx = grep("^===", menFiles[["2006"]])
separatorRow = menFiles[['2006']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
menFiles[['2006']][separatorIdx] = separatorRowX

menResMat = sapply(menFiles, extractVariables)
menDF = mapply(createDF, menResMat, year = 1999:2012,
               sex = rep("M", 14), SIMPLIFY = FALSE)

separatorIdx = grep("^===", womenFiles[["2006"]])
separatorRow = womenFiles[['2006']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
womenFiles[['2006']][separatorIdx] = separatorRowX

womenResMat = sapply(womenFiles, extractVariables)
womenDF = mapply(createDF, womenResMat, year = 1999:2012,
                 sex = rep("W", 14), SIMPLIFY = FALSE)

#pdf("CB_BoxplotTimeByYr.#pdf", width = 8, height = 5)

boxplot(sapply(menDF, function(x) x$runTime), 
        xlab = "Year", ylab = "Run Time (min)")
title(main = "Male Runners' Run Time Per Year")
boxplot(sapply(womenDF, function(x) x$runTime), 
        xlab = "Year", ylab = "Run Time (min)")
title(main = "Female Runners' Run Time Per Year")
dev.off()

# Combine race results across all years

cbMen = do.call(rbind, menDF)
save(cbMen, file = "cbMen.rda")
cbWomen = do.call(rbind, womenDF)
save(cbWomen, file = "cbWomen.rda")
dim(cbMen)
dim(cbWomen)