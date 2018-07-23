#### R Source File for Prepping Data for Analysis ####
#Yao, Kruse, Angelov

spamPath = "./spam"
list.files(path = paste(spamPath, "messages", sep = .Platform$file.sep))

head(list.files(path = paste(spamPath, "messages", "spam_2", sep = .Platform$file.sep)))

dirNames = list.files(path = paste(spamPath, "messages", sep = .Platform$file.sep))
length(list.files(paste(spamPath, "messages", dirNames, sep = .Platform$file.sep)))

sapply(paste(spamPath, "messages", dirNames, sep = .Platform$file.sep), function(dir) length(list.files(dir)))
fullDirNames = paste(spamPath, "messages", dirNames, sep = .Platform$file.sep)

fileNames = list.files(fullDirNames[1], full.names = TRUE)
fileNames[1]

msg = readLines(fileNames[1])
head(msg)

indx = c(1:5, 15, 27, 68, 69, 329, 404, 427, 516, 852, 971)
fn = list.files(fullDirNames[1], full.names = TRUE)[indx]
sampleEmail = sapply(fn, readLines)        

msg = sampleEmail[[1]]
splitPoint = match("", msg)

msg[ (splitPoint - 2):(splitPoint + 6) ]

header = msg[1:(splitPoint-1)]
body = msg[ -(1:splitPoint) ]

splitMessage = function(msg) {
  splitPoint = match("", msg)
  header = msg[1:(splitPoint-1)]
  body = msg[ -(1:splitPoint) ]
  return(list(header = header, body = body))
}

sampleSplit = lapply(sampleEmail, splitMessage)

header = sampleSplit[[1]]$header
grep("Content-Type", header)
grep("multi", tolower(header[46]))

headerList = lapply(sampleSplit, function(msg) msg$header)
CTloc = sapply(headerList, grep, pattern = "Content-Type")
sapply(headerList, function(header) {
  CTloc = grep("Content-Type", header)
  if (length(CTloc) == 0) return(NA)
  CTloc
})

hasAttach = sapply(headerList, function(header) {
  CTloc = grep("Content-Type", header)
  if (length(CTloc) == 0) return(FALSE)
  grepl("multi", tolower(header[CTloc])) 
})

header = sampleSplit[[6]]$header
boundaryIdx = grep("boundary=", header)

sub(".*boundary=\"(.*)\";.*", "\\1", header[boundaryIdx])

header2 = headerList[[9]]
boundaryIdx2 = grep("boundary=", header2)

sub('.*boundary="(.*)";.*', "\\1", header2[boundaryIdx2])
boundary2 = gsub('"', "", header2[boundaryIdx2])
sub(".*boundary= *(.*);?.*", "\\1", boundary2)
boundary = gsub('"', "", header[boundaryIdx])
sub(".*boundary= *(.*);?.*", "\\1", boundary)
sub(".*boundary= *([^;]*);?.*", "\\1", boundary)

getBoundary = function(header) {
  boundaryIdx = grep("boundary=", header)
  boundary = gsub('"', "", header[boundaryIdx])
  gsub(".*boundary= *([^;]*);?.*", "\\1", boundary)
}

boundary = getBoundary(headerList[[15]]) 
body = sampleSplit[[15]]$body

bString = paste("--", boundary, sep = "")
bStringLocs = which(bString == body)

eString = paste("--", boundary, "--", sep = "")
eStringLoc = which(eString == body)

msg = body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1)]
msg = c(msg, body[ (eStringLoc + 1) : length(body) ])

dropAttach = function(body, boundary){
  
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  
  if (length(bStringLocs) <= 1) return(body)
  
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  if (length(eStringLoc) == 0) 
    return(body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1)])
  
  n = length(body)
  if (eStringLoc < n) 
    return( body[ c( (bStringLocs[1] + 1) : (bStringLocs[2] - 1), ( (eStringLoc + 1) : n )) ] )
  return( body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1) ])
}

msg = sampleSplit[[3]]$body
msg[ c(1, 3, 26, 27) ]

cleanMsg = tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", msg))
cleanMsg[ c(1, 3, 26, 27) ]
stopWords = stopwords()
cleanSW = tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", stopWords))
SWords = unlist(strsplit(cleanSW, "[[:blank:]]+"))
SWords = SWords[ nchar(SWords) > 1 ]
stopWords = unique(SWords)

words = unlist(strsplit(cleanMsg, "[[:blank:]]+"))
words = words[ nchar(words) > 1 ]

words = words[ !( words %in% stopWords) ]
head(words)

cleanText = function(msg) {
  tolower(gsub("[[:punct:]0-9[:space:][:blank:]]+", " ", msg))
}

findMsgWords = function(msg, stopWords) {
  if(is.null(msg))
    return(character())
  words = unique(unlist(strsplit(cleanText(msg), "[[:blank:]\t]+")))
  # drop empty and 1 letter words
  words = words[ nchar(words) > 1]
  words = words[ !( words %in% stopWords) ]
  invisible(words)
}

processAllWords = function(dirName, stopWords)
{
  # read all files in the directory
  fileNames = list.files(dirName, full.names = TRUE)
  # drop files that are not email, i.e., cmds
  notEmail = grep("cmds$", fileNames)
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]
  messages = lapply(fileNames, readLines, encoding = "latin1")
  # split header and body
  emailSplit = lapply(messages, splitMessage)
  # put body and header in own lists
  bodyList = lapply(emailSplit, function(msg) msg$body)
  headerList = lapply(emailSplit, function(msg) msg$header)
  rm(emailSplit)
  # determine which messages have attachments
  hasAttach = sapply(headerList, function(header) {
    CTloc = grep("Content-Type", header)
    if (length(CTloc) == 0) return(0)
    multi = grep("multi", tolower(header[CTloc])) 
    if (length(multi) == 0) return(0)
    multi
  })
  
  hasAttach = which(hasAttach > 0)
  # find boundary strings for messages with attachments
  boundaries = sapply(headerList[hasAttach], getBoundary)
  # drop attachments from message body
  bodyList[hasAttach] = mapply(dropAttach, bodyList[hasAttach], boundaries, SIMPLIFY = FALSE)
  # extract words from body
  msgWordsList = lapply(bodyList, findMsgWords, stopWords)
  invisible(msgWordsList)
}

msgWordsList = lapply(fullDirNames, processAllWords, stopWords = stopWords)
numMsgs = sapply(msgWordsList, length)
numMsgs

isSpam = rep(c(FALSE, FALSE, FALSE, TRUE, TRUE), numMsgs)
msgWordsList = unlist(msgWordsList, recursive = FALSE)
numEmail = length(isSpam)
numSpam = sum(isSpam)
numHam = numEmail - numSpam