#### R Source File for Prepping Data for Analysis ####
#Yao, Kruse, Angelov

largeNum = 10000
print(sum(smallNums), digits = 20)
print(largeNum + sum(smallNums), digits = 20)

for (i in 1:length(smallNums)) {
  largeNum = largeNum + smallNums[i]
}
print(largeNum, digits = 20)
sampleSplit = lapply(sampleEmail, splitMessage)

header = sampleSplit[[1]]$header
header[1] = sub("^From", "Top-From:", header[1])

headerPieces = read.dcf(textConnection(header), all = TRUE)

headerVec = unlist(headerPieces)
dupKeys = sapply(headerPieces, function(x) length(unlist(x)))
names(headerVec) = rep(colnames(headerPieces), dupKeys)

headerVec[ which(names(headerVec) == "Delivered-To") ]
length(headerVec)
length(unique(names(headerVec)))

processHeader = function(header)
{
  # modify the first line to create a key:value pair
  header[1] = sub("^From", "Top-From:", header[1])
  
  headerMat = read.dcf(textConnection(header), all = TRUE)
  headerVec = unlist(headerMat)
  
  dupKeys = sapply(headerMat, function(x) length(unlist(x)))
  names(headerVec) = rep(colnames(headerMat), dupKeys)
  
  return(headerVec)
}

headerList = lapply(sampleSplit, function(msg) {processHeader(msg$header)} )

contentTypes = sapply(headerList, function(header) header["Content-Type"])
names(contentTypes) = NULL

hasAttach = grep("^ *multi", tolower(contentTypes))

boundaries = getBoundary(contentTypes[ hasAttach ])

boundary = boundaries[9]
body = sampleSplit[[15]]$body

bString = paste("--", boundary, sep = "")
bStringLocs = which(bString == body)

eString = paste("--", boundary, "--", sep = "")
eStringLoc = which(eString == body)

diff(c(bStringLocs[-1], eStringLoc))

### This code has mistakes in it - and we fix them later!
processAttach = function(body, contentType){
  boundary = getBoundary(contentType)
  bString = paste("--", boundary, "$", sep = "")
  bStringLocs = grep(bString, body)
  
  eString = paste("--", boundary, "--$", sep = "")
  eStringLoc = grep(eString, body)
  
  n = length(body)
  
  if (length(eStringLoc) == 0) eStringLoc = n + 1
  if (length(bStringLocs) == 1) attachLocs = NULL
  else attachLocs = c(bStringLocs[-1],  eStringLoc)
  
  msg = body[ (bStringLocs[1] + 1) : min(n, (bStringLocs[2] - 1), na.rm = TRUE)]
  
  if ( eStringLoc < n )
    msg = c(msg, body[ (eStringLoc + 1) : n ])
  
  if ( !is.null(attachLocs) ) {
    attachLens = diff(attachLocs, lag = 1) 
    attachTypes = mapply(function(begL, endL) {
      contentTypeLoc = grep("[Cc]ontent-[Tt]ype", body[ (begL + 1) : (endL - 1)])
      contentType = body[ begL + contentTypeLoc]
      contentType = gsub('"', "", contentType )
      MIMEType = sub(" *Content-Type: *([^;]*);?.*", "\\1", contentType)
      return(MIMEType)
    }, attachLocs[-length(attachLocs)], attachLocs[-1])
  }
  
  if (is.null(attachLocs)) return(list(body = msg, attachInfo = NULL) )
  else return(list(body = msg, attachDF = data.frame(aLen = attachLens, aType = attachTypes, stringsAsFactors = FALSE)))
}

bodyList = lapply(sampleSplit, function(msg) msg$body)
attList = mapply(processAttach, bodyList[hasAttach], contentTypes[hasAttach], SIMPLIFY = FALSE)
lens = sapply(attList, function(processedA) processedA$attachDF$aLen)
body = bodyList[hasAttach][[2]]

processAttach = function(body, contentType){
  
  n = length(body)
  boundary = getBoundary(contentType)
  
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  
  if (length(eStringLoc) == 0) eStringLoc = n
  if (length(bStringLocs) <= 1) {
    attachLocs = NULL
    msgLastLine = n
    if (length(bStringLocs) == 0) bStringLocs = 0
  } else {
    attachLocs = c(bStringLocs[ -1 ],  eStringLoc)
    msgLastLine = bStringLocs[2] - 1
  }
  
  msg = body[ (bStringLocs[1] + 1) : msgLastLine] 
  if ( eStringLoc < n )
    msg = c(msg, body[ (eStringLoc + 1) : n ])
  
  if ( !is.null(attachLocs) ) {
    attachLens = diff(attachLocs, lag = 1) 
    attachTypes = mapply(function(begL, endL) {
      CTloc = grep("^[Cc]ontent-[Tt]ype", body[ (begL + 1) : (endL - 1)])
      if ( length(CTloc) == 0 ) {
        MIMEType = NA
      } else {
        CTval = body[ begL + CTloc[1] ]
        CTval = gsub('"', "", CTval )
        MIMEType = sub(" *[Cc]ontent-[Tt]ype: *([^;]*);?.*", "\\1", CTval)   
      }
      return(MIMEType)
    }, attachLocs[-length(attachLocs)], attachLocs[-1])
  }
  
  if (is.null(attachLocs)) return(list(body = msg, attachDF = NULL) )
  return(list(body = msg, attachDF = data.frame(aLen = attachLens, aType = unlist(attachTypes), stringsAsFactors = FALSE)))
}                       

readEmail = function(dirName) {
  # retrieve the names of files in directory
  fileNames = list.files(dirName, full.names = TRUE)
  # drop files that are not email
  notEmail = grep("cmds$", fileNames)
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]
  # read all files in the directory
  lapply(fileNames, readLines, encoding = "latin1")
}

processAllEmail = function(dirName, isSpam = FALSE)
{
  # read all files in the directory
  messages = readEmail(dirName)
  fileNames = names(messages)
  n = length(messages)
  
  # split header from body
  eSplit = lapply(messages, splitMessage)
  rm(messages)
  
  # process header as named character vector
  headerList = lapply(eSplit, function(msg) processHeader(msg$header))
  
  # extract content-type key
  contentTypes = sapply(headerList, function(header) header["Content-Type"])
  
  # extract the body
  bodyList = lapply(eSplit, function(msg) msg$body)
  rm(eSplit)
  
  # which email have attachments
  hasAttach = grep("^ *multi", tolower(contentTypes))
  
  # get summary stats for attachments and the shorter body
  attList = mapply(processAttach, bodyList[hasAttach], contentTypes[hasAttach], SIMPLIFY = FALSE)
  
  bodyList[hasAttach] = lapply(attList, function(attEl) attEl$body)
  
  attachInfo = vector("list", length = n )
  attachInfo[ hasAttach ] = lapply(attList, function(attEl) attEl$attachDF)
  
  # prepare return structure
  emailList = mapply(function(header, body, attach, isSpam) {
    list(isSpam = isSpam, header = header, body = body, attach = attach)
  },
  headerList, bodyList, attachInfo, rep(isSpam, n), SIMPLIFY = FALSE)
  names(emailList) = fileNames
  invisible(emailList)
}

emailStruct = mapply(processAllEmail, fullDirNames, isSpam = rep( c(FALSE, TRUE), 3:2))      
emailStruct = unlist(emailStruct, recursive = FALSE)
sampleStruct = emailStruct[ indx ]

save(emailStruct, file="emailXX.rda")

header = sampleStruct[[1]]$header
subject = header["Subject"]
els = strsplit(subject, "")
all(els %in% LETTERS)

testSubject = c("DEAR MADAME", "WINNER!", "")

els = strsplit(testSubject, "")
sapply(els, function(subject) all(subject %in% LETTERS))

gsub("[[:punct:] ]", "", testSubject)
gsub("[^[:alpha:]]", "", testSubject)

isYelling = function(msg) {
  if ( "Subject" %in% names(msg$header) ) {
    el = gsub("[^[:alpha:]]", "", msg$header["Subject"])
    if (nchar(el) > 0) 
      nchar(gsub("[A-Z]", "", el)) < 1
    else 
      FALSE
  } else 
    NA
}

perCaps = function(msg)
{
  body = paste(msg$body, collapse = "")
  
  # Return NA if the body of the message is "empty"
  if(length(body) == 0 || nchar(body) == 0) return(NA)
  
  # Eliminate non-alpha characters
  body = gsub("[^[:alpha:]]", "", body)
  capText = gsub("[^A-Z]", "", body)
  100 * nchar(capText)/nchar(body)
}

funcList = list( 
  isRe = function(msg) {
    "Subject" %in% names(msg$header) && length(grep("^[ \t]*Re:", msg$header[["Subject"]])) > 0
  },
  numLines = function(msg) length(msg$body),
  isYelling = function(msg) {
    if ( "Subject" %in% names(msg$header) ) {
      el = gsub("[^[:alpha:]]", "", msg$header["Subject"])
      if (nchar(el) > 0) 
        nchar(gsub("[A-Z]", "", el)) < 1
      else 
        FALSE
    }
    else NA
  },
  perCaps = function(msg) {
    body = paste(msg$body, collapse = "")
    
    # Return NA if the body of the message is "empty"
    if(length(body) == 0 || nchar(body) == 0) return(NA)
    
    # Eliminate non-alpha characters
    body = gsub("[^[:alpha:]]", "", body)
    capText = gsub("[^A-Z]", "", body)
    100 * nchar(capText)/nchar(body)
  }
)

lapply(funcList, function(func) sapply(sampleStruct, function(msg) func(msg)))

createDerivedDF = function(email = emailStruct, operations = funcList, verbose = FALSE)
{
  els = lapply(names(operations),
               function(id) {
                 if(verbose) print(id)
                 e = operations[[id]]
                 v = if(is.function(e)) 
                   sapply(email, e)
                 else 
                   sapply(email, function(msg) eval(e))
               })
  df = as.data.frame(els)
  names(df) = names(operations)
  invisible(df)
}

sampleDF = createDerivedDF(sampleStruct)

funcList = list(
  isSpam = expression(msg$isSpam),
  isRe = function(msg) {
    # Can have a Fwd: Re:  ... but we are not looking for this here.
    # We may want to look at In-Reply-To field.
    "Subject" %in% names(msg$header) && length(grep("^[ \t]*Re:", msg$header[["Subject"]])) > 0
  },
  numLines = function(msg) length(msg$body),
  bodyCharCt = function(msg) sum(nchar(msg$body)),
  underscore = function(msg) {
    if(!"Reply-To" %in% names(msg$header))
      return(FALSE)
    
    txt <- msg$header[["Reply-To"]]
    length(grep("_", txt)) > 0  && length(grep("[0-9A-Za-z]+", txt)) > 0
  }
  ,
  subExcCt = function(msg) {
    x = msg$header["Subject"]
    if(length(x) == 0 || sum(nchar(x)) == 0 || is.na(x))
      return(NA)
    
    sum(nchar(gsub("[^!]","", x)))
  }
  ,
  subQuesCt = function(msg) {
    x = msg$header["Subject"]
    if(length(x) == 0 || sum(nchar(x)) == 0 || is.na(x))
      return(NA)
    
    sum(nchar(gsub("[^?]","", x)))
  }
  ,
  numAtt = function(msg) {
    if (is.null(msg$attach)) return(0)
    else nrow(msg$attach)
  }
  ,
  priority = function(msg) {
    ans <- FALSE
    # Look for names X-Priority, Priority, X-Msmail-Priority
    # Look for high any where in the value
    ind = grep("priority", tolower(names(msg$header)))
    if (length(ind) > 0)  {
      ans <- length(grep("high", tolower(msg$header[ind]))) >0
    }
    ans
  }
  ,
  numRec = function(msg) {
    # unique or not.
    els = getMessageRecipients(msg$header)
    
    if(length(els) == 0)
      return(NA)
    
    # Split each line by ","  and in each of these elements, look for
    # the @ sign. This handles
    tmp = sapply(strsplit(els, ","), function(x) grep("@", x))
    sum(sapply(tmp, length))
  }
  ,
  perCaps = function(msg) {
    body = paste(msg$body, collapse = "")
    
    # Return NA if the body of the message is "empty"
    if(length(body) == 0 || nchar(body) == 0) return(NA)
    
    # Eliminate non-alpha characters and empty lines 
    body = gsub("[^[:alpha:]]", "", body)
    els = unlist(strsplit(body, ""))
    ctCap = sum(els %in% LETTERS)
    100 * ctCap / length(els)
  }
  ,
  isInReplyTo = function(msg) {
    "In-Reply-To" %in% names(msg$header)
  }
  ,
  sortedRec = function(msg) {
    ids = getMessageRecipients(msg$header)
    all(sort(ids) == ids)
  }
  ,
  subPunc = function(msg) {
    if("Subject" %in% names(msg$header)) {
      el = gsub("['/.:@-]", "", msg$header["Subject"])
      length(grep("[A-Za-z][[:punct:]]+[A-Za-z]", el)) > 0
    }
    else
      FALSE
  },
  hour = function(msg) {
    date = msg$header["Date"]
    if ( is.null(date) ) return(NA)
    # Need to handle that there may be only one digit in the hour
    locate = regexpr("[0-2]?[0-9]:[0-5][0-9]:[0-5][0-9]", date)
    
    if (locate < 0)
      locate = regexpr("[0-2]?[0-9]:[0-5][0-9]", date)
    if (locate < 0) return(NA)
    
    hour = substring(date, locate, locate+1)
    hour = as.numeric(gsub(":", "", hour))
    
    locate = regexpr("PM", date)
    if (locate > 0) hour = hour + 12
    
    locate = regexpr("[+-][0-2][0-9]00", date)
    if (locate < 0) offset = 0
    else offset = as.numeric(substring(date, locate, locate + 2))
    (hour - offset) %% 24
  }
  ,
  multipartText = function(msg) {
    if (is.null(msg$attach)) return(FALSE)
    numAtt = nrow(msg$attach)
    types = length(grep("(html|plain|text)", msg$attach$aType)) > (numAtt/2)
  }
  ,
  hasImages = function(msg) {
    if (is.null(msg$attach)) return(FALSE)
    length(grep("^ *image", tolower(msg$attach$aType))) > 0
  }
  ,
  isPGPsigned = function(msg) {
    if (is.null(msg$attach)) return(FALSE)
    length(grep("pgp", tolower(msg$attach$aType))) > 0
  }
  ,
  perHTML = function(msg) {
    if(! ("Content-Type" %in% names(msg$header))) return(0)
    
    el = tolower(msg$header["Content-Type"]) 
    if (length(grep("html", el)) == 0) return(0)
    
    els = gsub("[[:space:]]", "", msg$body)
    totchar = sum(nchar(els))
    totplain = sum(nchar(gsub("<[^<]+>", "", els )))
    100 * (totchar - totplain)/totchar
  }
  ,
  subSpamWords = function(msg) {
    if("Subject" %in% names(msg$header))
      length(grep(paste(SpamCheckWords, collapse = "|"), tolower(msg$header["Subject"]))) > 0
    else
      NA
  }
  ,
  subBlanks = function(msg) {
    if("Subject" %in% names(msg$header)) {
      x = msg$header["Subject"]
      # should we count blank subject line as 0 or 1 or NA?
      if (nchar(x) == 1) return(0)
      else 100 *(1 - (nchar(gsub("[[:blank:]]", "", x))/nchar(x)))
    } else NA
  }
  ,
  noHost = function(msg)
  {
    # Or use partial matching.
    idx = pmatch("Message-", names(msg$header))
    
    if(is.na(idx)) return(NA)
    
    tmp = msg$header[idx]
    return(length(grep(".*@[^[:space:]]+", tmp)) ==  0)
  }
  ,
  numEnd = function(msg) {
    # If we just do a grep("[0-9]@",  )
    # we get matches on messages that have a From something like
    # " \"marty66@aol.com\" <synjan@ecis.com>"
    # and the marty66 is the "user's name" not the login
    # So we can be more precise if we want.
    x = names(msg$header)
    if ( !( "From" %in% x) ) return(NA)
    login = gsub("^.*<", "", msg$header["From"])
    if ( is.null(login) ) 
      login = gsub("^.*<", "", msg$header["X-From"])
    if ( is.null(login) ) return(NA)
    login = strsplit(login, "@")[[1]][1]
    length(grep("[0-9]+$", login)) > 0
  },
  isYelling = function(msg) {
    if ( "Subject" %in% names(msg$header) ) {
      el = gsub("[^[:alpha:]]", "", msg$header["Subject"])
      if (nchar(el) > 0) nchar(gsub("[A-Z]", "", el)) < 1
      else FALSE
    }
    else
      NA
  },
  forwards = function(msg) {
    x = msg$body
    if(length(x) == 0 || sum(nchar(x)) == 0)
      return(NA)
    ans = length(grep("^[[:space:]]*>", x))
    100 * ans / length(x)
  },
  isOrigMsg = function(msg) {
    x = msg$body
    if(length(x) == 0) return(NA)
    length(grep("^[^[:alpha:]]*original[^[:alpha:]]+message[^[:alpha:]]*$", tolower(x) ) ) > 0
  },
  isDear = function(msg) {
    x = msg$body
    if(length(x) == 0) return(NA)
    length(grep("^[[:blank:]]*dear +(sir|madam)\\>", tolower(x))) > 0
  },
  isWrote = function(msg) {
    x = msg$body
    if(length(x) == 0) return(NA)
    length(grep("(wrote|schrieb|ecrit|escribe):", tolower(x) )) > 0
  },
  avgWordLen = function(msg) {
    txt = paste(msg$body, collapse = " ")
    if(length(txt) == 0 || sum(nchar(txt)) == 0) return(0)
    txt = gsub("[^[:alpha:]]", " ", txt)
    words = unlist(strsplit(txt, "[[:blank:]]+"))
    wordLens = nchar(words)
    mean(wordLens[ wordLens > 0 ])
  }
  ,
  numDlr = function(msg) {
    x = paste(msg$body, collapse = "")
    if(length(x) == 0 || sum(nchar(x)) == 0)
      return(NA)
    nchar(gsub("[^$]","", x))
  }
)

SpamCheckWords =
  c("viagra", "pounds", "free", "weight", "guarantee", "million", 
    "dollars", "credit", "risk", "prescription", "generic", "drug",
    "financial", "save", "dollar", "erotic", "million", "barrister",
    "beneficiary", "easy", "money back", "money", "credit card")

getMessageRecipients = function(header) {
  c(if("To" %in% names(header))  header[["To"]] else character(0),
    if("Cc" %in% names(header))  header[["Cc"]] else character(0),
    if("Bcc" %in% names(header)) header[["Bcc"]] else character(0))
}

emailDF = createDerivedDF(emailStruct)

perCaps2 = function(msg) {
  body = paste(msg$body, collapse = "")
  # Return NA if the body of the message is "empty"
  if(length(body) == 0 || nchar(body) == 0) return(NA)
  
  # Eliminate non-alpha characters and empty lines 
  body = gsub("[^[:alpha:]]", "", body)
  els = unlist(strsplit(body, ""))
  ctCap = sum(els %in% LETTERS)
  100 * ctCap / length(els)
}

pC = sapply(emailStruct, perCaps)
pC2 = sapply(emailStruct, perCaps2)
identical(pC, pC2)

indNA = which(is.na(emailDF$subExcCt))
indNoSubject = which(sapply(emailStruct, function(msg) !("Subject" %in% names(msg$header))))
all(indNA == indNoSubject)
all(emailDF$bodyCharCt > emailDF$numLines)

x.at = c(1,10,100,1000,10000,100000)
y.at = c(1, 5, 10, 50, 100, 500, 5000)
nL = 1 + emailDF$numLines
nC = 1 + emailDF$bodyCharCt