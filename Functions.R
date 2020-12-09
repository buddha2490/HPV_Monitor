downloadAzure <- function(loc) {
  files <- list_azure_files(loc) %>%
    filter(isdir == "FALSE") %>%
    filter(name != ".DB")
  
  
  if (dir.exists("db")) {
    z <- list.files("db")
    lapply(z, function(x) file.remove(file.path("db",x)))
    unlink("db", recursive = T)
  }
  
  dir.create("db")
  
  mclapply(files$name, function(x) {
    storage_download(loc, x, overwrite = T)
  }, mc.cores = getOption("mc.cores", detectCores()))
  
  mclapply(files$name, function(x) {
    file.copy(from = x,
              to = file.path("db", x))
    file.remove(x)
  })
  
}



mergeDataForMonitor <- function() {
  
  files <- list.files("db", pattern = ".DB")
  
# Merge the data
  tbls <- c("demographics", "systems", "savedRates", "savedActivities", "followupRates", "additionalInfo")
  myData <- mclapply(files, function(x) {
    conn <- dbConnect(SQLite(), file.path("db",x)) # connect to the database
    foo <- lapply(tbls, function(y) {
      df <- dbReadTable(conn, y) # read the table
      dateVar <- names(df)[2]
      # Get the most recent version
      df <- df[order(df[[dateVar]], decreasing = T),]
      df[[dateVar]] <- as.POSIXct(df[[dateVar]], origin = "1970-01-01")
      return(df[1,])
    }) %>%
      Reduce(function(x,y) full_join(x,y,"Username"), .)
  }, mc.cores = getOption("mc.cores", detectCores())) %>% 
    do.call("rbind",.)

  return(myData)
}





# Prepare the dataset -----------------------------------------------------

prep <- function(dat) {
 
  df <- dat
  

# Regions - based on state (Q4b) ------------------------------------------


  west <- c("AK","OR","WA","NV","CA","HI","GU")
  north <- c("ID","MT","WY","UT","CO","ND","SD","NE","KS","MN","IA","MO","WI")
  south <- c("AZ","NM","TX","OK","AR","LA","MS","AL")
  northCentral <- c("MI","IL","IN","OH","KY","WV","TN")
  southEast <- c("PR","FL","GA","SC","NC","VA")
  northEast <- c("DC","MD","DE","NJ","PA","NY","RI","CT","MA","NH","VT","ME")
  df$Region <- with(df, ifelse(
    is.na(Q4b) | Q4b == "", "State not provided", ifelse(
    Q4b %in% west, "West", ifelse(
      Q4b %in% north, "North", ifelse(
        Q4b %in% south, "South", ifelse(
          Q4b %in% northCentral, "North Central", ifelse(
            Q4b %in% southEast, "Southeast", ifelse(
              Q4b %in% northEast, "Northeast", "Left blank")))))))) %>%
    factor(c("West", "North", "South", "North Central", "Southeast", "Northeast", "State not provided"))
  
  
  
  # Initial Formatting - mostly dates ---------------------------------------
  
  df$demographicsDate <- as.Date(df$demographicsDate, origin = "1970-01-01")
  df$Q13_date <- as.Date(df$Q13_date, origin = "1970-01-01")
  df$systemsDate <- as.Date(df$systemsDate, origin = "1970-01-01")
  df$baseratesDate <- as.Date(df$baseratesDate, origin = "1970-01-01")
  df$activitiesDate <- as.Date(df$activitiesDate, origin = "1970-01-01")
  df$fwupratesDate <- as.Date(df$fwupratesDate, origin = "1970-01-01")
  df$additionalInfoDate <- as.Date(df$additionalInfoDate, origin = "1970-01-01")
  
  
  # Restructure the whole damn thing as a list
  # Each element of the list is one variable
  # That way the multiple options can be long form
  # everything else will just be pretty simple
  # Make each list 2 variables:  Username and the Variable
  
  
  
  
  # No Include --------------------------------------------------------------
  # These are the textgroup boxes and don't need to be included in frequencies
  # I will deal with the individual levels later
  noInclude <- c("Q11", "Q12", "Q14", "Q17","Q18", "Q25", "Q28", 
                 "Q32_details", "Q35", "Q37", "Q38", "Q39", "Q2FU", 
                 "Q7FU", "Q8FU", "Q9FU", "Q13FU", "Q14FU")
  
  
  
  
  # Make a list of the multi-group variables ---------------------------------
  
  multiFreq <- list()
  
  # Q11 - don't include
  Q11 <- c("Q11_1", "Q11_2", "Q11_3", "Q11_4", "Q11_5")
  
  # Q12 - don't include
  Q12 <- c("Q12_1", "Q12_2", "Q12_3", "Q12_4", "Q12_5", "Q12_6")
  
  # Q14 - don't include
  Q14 <- c("Q14_1", "Q14_2", "Q14_3")
  
  # Q17 - don't include
  Q17 <- c("Q17_1", "Q17_2", "Q17_3", "Q17_4", "Q17_5")
  
  # Q18 - don't include
  Q18 <- c("Q19_1", "Q19_2", "Q19_3", "Q19_4", "Q19_5")
  
  # Q25 - don't include
  Q25 <- c("Q25_1", "Q25_2", "Q25_3", "Q25_4", "Q25_5")
  
  # Q26 - don't include
  Q26 <- c("Q26_1", "Q26_2", "Q26_3", "Q26_4", "Q26_5", "Q26_6",
           "Q26_7", "Q26_8", "Q26_9")
  
  # Q28 - don't include
  Q28 <- c("Q28_1", "Q28_2", "Q28_3", "Q28_4")
  
  # Q32_details - don't include
  Q32_details <- c("Q32_details1", "Q32_details2", "Q32_details3", "Q32_details4")
  
  # Q35 - don't include
  Q35 <- c("Q35_1", "Q35_2", "Q35_3", "Q35_4")
  
  # Q37 - don't include
  Q37 <- c("Q37_1", "Q37_2", "Q37_3", "Q37_4")
  
  # Q38 - don't include
  Q38 <- c("Q38_1", "Q38_2", "Q38_3", "Q38_4", "Q38_5", 
           "Q38_6", "Q38_7", "Q38_8", "Q38_9")
  
  # Q39 - don't include
  Q39 <- c("Q39_1", "Q39_2", "Q39_3", "Q39_4", "Q39_5")
  
  # Q2FU - don't include
  Q2FU <- c("Q2FU_1", "Q2FU_2", "Q2FU_3", "Q2FU_4")
  
  # Q7FU
  Q7FU <- c("Q7FU_1", "Q7FU_2", "Q7FU_3", "Q7FU_4", "Q7FU_5")
  
  # Q8FU
  Q8FU <- c("Q8FU_1", "Q8FU_2", "Q8FU_3")
  
  # Q9FU
  Q9FU <- c("Q9FU_1", "Q9FU_2", "Q9FU_3", "Q9FU_4", "Q9FU_5", 
            "Q9FU_6", "Q9FU_7", "Q9FU_8", "Q9FU_9")
  
  # Q13FU
  Q13FU <- c("Q13FU_1", "Q13FU_2", "Q13FU_3", "Q13FU_4", "Q13FU_5", 
             "Q13FU_6", "Q13FU_7", "Q13FU_8")
  
  # Q14FU
  Q14FU <- c("Q14FU_1", "Q14FU_2", "Q14FU_3", "Q14FU_4", "Q14FU_5")
  
  multi <- df[,c("Username",Q11, Q12, Q14, Q17, Q18, Q25, Q26, Q28, Q32_details, 
                 Q35, Q37, Q38, Q39, Q2FU, Q7FU, Q8FU, Q9FU, Q13FU, Q14FU)]
  
  
  
  
  # Make a list of all text-entry variables -----
  
  # Make a list that need listing but not frequencies
  
  stringVars <- c("HealthSystem",
                  "Q1_DBA",
                  "Q4a",
                  "Q4c",
                  "Q5",
                  "Q5_Email",
                  "Q6",
                  "Q7",
                  "Q8",
                  "Q8_Email",
                  "Q9",
                  "Q11_other",
                  "Q12_other",
                  "Q13_amount",
                  "Q13_source",
                  "Q14_other",
                  "Q20_notes",
                  "Q20_orders",
                  "Q20_other",
                  "Q21_text",
                  "Q22_notes",
                  "Q23_notes",
                  "Q25_other",
                  "Q26_other",
                  "Q27_other",
                  "Q28_other",
                  "Q30",
                  "Q31_other",
                  "Q32_other",
                  "Q33_other",
                  "Q35_other",
                  "Q36a",
                  "Q36b",
                  "Q36c",
                  "Q36d",
                  "Q36e",
                  "Q36f",
                  "Q38_other",
                  "Q39_other",
                  "Q40",
                  "Q41",
                  "Q42",
                  paste0("act",1:10),
                  paste0("time",1:10),
                  paste0("ppl",1:10),
                  "Q1FU_other",
                  "Q2FU_other",
                  "Q4FU",
                  "Q5FU_text",
                  "Q6FU_text",
                  "Q7FU_other",
                  "Q8FU_other",
                  "Q9FU_other",
                  "Q13FU_other",
                  "Q14FU_other",
                  "Q15FU",
                  "Q16FU",
                  "Q17FU",
                  "Q18FU_more")
  
  strings <- df[,c("Username",stringVars)]
  
  
  
  # Make a list of variables to just drop from the frequencies --------------
  multiDrop <- c("Q11", "Q12", "Q14", "Q17", "Q18", "Q25", "Q26", "Q28", "Q32_details", 
                 "Q35", "Q37", "Q38", "Q39", "Q2FU", "Q7FU", "Q8FU", "Q9FU", "Q13FU", "Q14FU")
  
  
  
  # Frequency Dataset -------------------------------------------------------
  # These are the data that actually need processing
  drop <- c(stringVars, multiDrop, Q11, Q12, Q14, Q17, Q18, Q25, Q26, Q28, Q32_details, 
            Q35, Q37, Q38, Q39, Q2FU, Q7FU, Q8FU, Q9FU, Q13FU, Q14FU)
  finalData <- dplyr::select(df, -all_of(drop)) # 119 variables
  
  
  
  # Process the data --------------------------------------------------------
  
  # First do the multi things
  reshapeDf <- function(vars) {
    bar <- multi[,c("Username", vars)]
    lapply(bar$Username, function(x) {
      one <- filter(bar, Username == x)
      responses <- as.character(one[,vars])
      data.frame(Username = x,
                 Variable = responses)
    }) %>%
      do.call("rbind",.)
  }
  
  # This starts my list
  dataList <- lapply(names(finalData)[-1], function(x) {
    data.frame(Username = finalData$Username, Variable = finalData[[x]])
  })
  names(dataList) <- names(finalData)[-1]
  rm(finalData)
  
  
  # Add the milti stuff
  dataList$Q11 <- reshapeDf(Q11)
  dataList$Q12 <- reshapeDf(Q12)
  dataList$Q14 <- reshapeDf(Q14)
  dataList$Q17 <- reshapeDf(Q17)
  dataList$Q18 <- reshapeDf(Q18)
  dataList$Q25 <- reshapeDf(Q25)
  dataList$Q26 <- reshapeDf(Q26)
  dataList$Q28 <- reshapeDf(Q28)
  dataList$Q32_details <- reshapeDf(Q32_details)
  dataList$Q35 <- reshapeDf(Q35)
  dataList$Q37 <- reshapeDf(Q37)
  dataList$Q38 <- reshapeDf(Q38)
  dataList$Q39 <- reshapeDf(Q39)
  dataList$Q2FU <- reshapeDf(Q2FU)
  dataList$Q7FU <- reshapeDf(Q7FU)
  dataList$Q8FU <- reshapeDf(Q8FU)
  dataList$Q9FU <- reshapeDf(Q9FU)
  dataList$Q13FU <- reshapeDf(Q13FU)
  dataList$Q14FU <- reshapeDf(Q14FU)
  
  # Then for the string vars, I'll just list them, but give them a new class
  stringNames <- names(strings)[-1]
  
  stringList <- lapply(stringNames, function(x) {
    l <- data.frame(Username = strings$Username,
                    Variable = strings[[x]])
    class(l$Variable) <- "strings"
    return(l)
  })
  names(stringList) <- stringNames
  
  dataList <- c(dataList, stringList)
  rm(multi, multiFreq, stringList, strings,drop, multiDrop, noInclude,
     stringVars, Q11, Q12, Q14, Q17, Q18, Q25, Q26, Q28, Q32_details, 
     Q35, Q37, Q38, Q39, Q2FU, Q7FU, Q8FU, Q9FU, Q13FU, Q14FU,
     stringNames)
  
  

# Add health system to these dataLists ------------------------------------
dataList <- lapply(dataList, function(x) {
  left_join(x, 
            dplyr::rename(dataList$HealthSystem, HealthSystem = Variable),
            "Username")
})
  

  
  
  
  # Prep Grouping Variables -------------------------------------------------
  # These will be cross tab variables that are merged back in with the rest
  # Delete the sample() stuff, that's for testing
  SystemType <- dataList$Q2
  SystemType$Variable <- with(SystemType, ifelse(
    is.na(Variable) | Variable == "", "Not provided", Variable)) %>%
                factor(levels = c("FQHC",
                                 "IDS",
                                 "Other (specify)",
                                 "Not provided"))
  dataList$SystemType <- dplyr::select(SystemType, Username, SystemType=Variable)
  
  
  clinic <- dataList$Q12
  clinic$Variable <- with(clinic, ifelse(
    is.na(Variable) | Variable == "", "Not provided", Variable)) %>%
    factor(levels = c("Urban clinics", 
                      "Suburban clinics",
                      "Rural clinics",
                      "School-based clinics",
                      "Mobile clinics",
                      "Other (specify)",
                      "Not provided"))
  dataList$Clinic <- dplyr::select(clinic, Username, Clinic = Variable)
  
  
  subcohort <- dataList$Q11
  subcohort$Variable <- with(subcohort, ifelse(
    is.na(Variable) | Variable == "", "Not provided", Variable)) %>%
    factor(levels = c("Pediatric",
                      "Internal Medicine",
                      "Family Medicine",
                      "Dental",
                      "Other (specify)",
                      "Not provided"))
  dataList$Subcohort <- dplyr::select(subcohort, Username, Subcohort = Variable)
  
  dataList$Region <- rename(dataList$Region, Region = Variable)

  return(dataList)
}



# Create the figure -------------------------------------------------------


processVars <- function(dat = dataList, dataVar,  filterVar){

  dataList <- dat
  foo <- dataList[[dataVar]]

  continuous <- function() {
    if (ncol(fool) == 3) {
      tbl <- summary(fool$Variable) %>%
        t() %>%
        data.frame() %>%
        dplyr::select(Q = Var2, Value = Freq)
      names(tbl) <- c("Quantile", "Value")
      tbl$Quantile <- as.character(tbl$Quantile)
      tbl$Quantile[tbl$Quantile == "NA's"] <- "N-Missing"
    } else {
      strata <- names(fool)[4]
      tbl <- lapply(levels(fool[[strata]]), function(x) {
        tmp <- fool[fool[[strata]] == x & !duplicated(fool$Username),]
        tbl <- summary(fool$Variable) %>%
          t() %>%
          data.frame() %>%
          dplyr::select(Q = Var2, Value = Freq)
        names(tbl) <- c("Quantile", "Value")
        return(tbl)
      })
      names(tbl) <- levels(fool[[strata]])
    }
    return(tbl)
  }
  categorical <- function() {
    if (ncol(fool) == 3) {
      tbl <- table(foo$Variable, useNA="ifany") %>%
        as.data.frame()
      tbl$Prop <- (tbl$Freq / nrow(foo)) * 100
      tbl$Final <- paste0(tbl$Freq, " (",
                          format(round(tbl$Prop,1), 1), ")")
      tbl <- tbl[,c("Var1", "Final")]
      names(tbl) <- c(dataVar, "N (%)")
    } else {
      strata <- names(fool)[4]
      tbl <- lapply(levels(fool[[strata]]), function(x) {
        fool2 <- fool[fool[[strata]] == x & !duplicated(fool$Username),]
        freq <- table(fool2$Variable, useNA="ifany") %>%
          as.data.frame()
        freq$Prop <- (freq$Freq / nrow(fool2)) * 100
        if (nrow(freq) == 0){
          freq <- data.frame(Var1 = "", Freq = 0, Prop = 100)
        }
        freq$Final <- paste0(freq$Freq, " (",
                             format(round(freq$Prop,1), 1), ")")
        freq <- freq[,c("Var1", "Final")]
        names(freq) <- c(dataVar, "N (%)")
        return(freq)
      })
      names(tbl) <- levels(fool[[strata]])
    }
    return(tbl)
  }
  string <- function(){
  return(dplyr::select(fool, HealthSystem, Response = Variable))
 }
  
  masterFun <- function(){ 
    if (class(dataList[[dataVar]]$Variable) == "numeric") return(continuous())
    if (class(dataList[[dataVar]]$Variable) == "Date") return(continuous())
    if (class(dataList[[dataVar]]$Variable) == "character") return(categorical())
    if (class(dataList[[dataVar]]$Variable) == "strings") return(string())
  }
  
 
  # Calculate all samples to combine with subgroups
  fool <- foo
  allsamples <- masterFun()


  # Now do each group
  if (filterVar == "All samples") {
    fool <- foo
    out <- masterFun()
  }
  if (filterVar == "By system type") {
    fool <- left_join(foo, dataList[["SystemType"]], "Username")
    out <- masterFun() # length = 4
  }
  if (filterVar == "By ACS region") {
    fool <- left_join(foo, 
                      dplyr::select(dataList[["Region"]], -HealthSystem), 
                      "Username")
    out <- masterFun()
  }
  if (filterVar == "By subcohort")  {
    fool <- left_join(foo, dataList[["Subcohort"]], "Username")
    out <- masterFun()
  }
  if (filterVar == "By clinic type")  {
    fool <- left_join(foo, dataList[["Clinic"]], "Username")
    out <- masterFun()
  }

  return(list(allsamples = allsamples, out = out))
  
}
