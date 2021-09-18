#' @title WhaleInjuryCovariates()
#'
#' @description
#' Identify Covariates for Whale Injury Assessments from
#' Narratives + Append Covariates to existing data.frame.
#'
#' Search wide-form data.frame column named 'Narrative' for words/phrases
#' to be coded as binary absence / presence (0/1) injury covariates.
#' Append to new data.frame with function WhaleInjuryCovariates().
#'
#' Resulting data.frame is used with randomForest models to predict
#' health status of whales involved in entanglements or vessel strikes.
#'
#' Package includes data.frame 'LargeWhaleData'
#'
#' @usage
#' WhaleInjuryCovariates(x)
#'
#' @param x an object of class 'data.frame'
#'
#' @author Jim Carretta <jim.carretta@noaa.gov>
#'
#' @examples WhaleInjuryCovariates(LargeWhaleData)
#'
#' @export

   WhaleInjuryCovariates = function(x) {

# 09-13-2021
# Covariates defined below, starting with covariate = 'anchored'
# Multiple words/phrases may be pooled into a single covariate, e.g. the covariate 'decline'
# includes narrative words/phrases 'cyamid', 'whale lice', 'emaciation', 'skin discoloration', etc.
# Covariate string searches use function 'grepl' and may be abbreviated to accommodate different
# states of same word/phrase/meaning. For example, an emaciated whale may include narrative
# references to: 'emacatied', 'emaciation'. Thus, the string 'emaciat' is used with function grepl.
# Covariate misspellings (e.g. 'propeller' vs 'propellar') are coded generically ('propell') to capture
# all occurrences.

# Multiple conditions are identified using string wildcards with grepl() function:
# Narratives including both 'flukes' and 'missing' are identified using grepl("missing.*fluke|fluke.*missing))
# while phrases as 'partially disentangled' and 'partial disentanglement' are handled by grepl("partial.*disentangl)

# Evidence the whale was anchored or had limited mobility resulting from an entanglement?
   anchored = grepl("anchor|motionless|unable to move|stationary|entrap|weighted", x$Narrative, ignore.case=TRUE)
    anchored = as.numeric(lapply(anchored, as.numeric))

# Was a calf, juvenile or lactating mother involved in the case?
    calf.juv = grepl("calf|juvenile|young|dependent", x$Narrative, ignore.case=TRUE)
     calf.juv = as.numeric(lapply(calf.juv, as.numeric))

# Evidence of constricting entanglement?
     constricting = grepl("constricting|tight|cutting|impress|embed|pinn|twisted|necrotic|amputat|missing.*fluke|fluke.*missing", x$Narrative, ignore.case=TRUE)
      constricting = as.numeric(lapply(constricting, as.numeric))

# Evidence of a health decline?
    decline = grepl(" abnormal|chronic|deteriorat|fair|compromise|scoliosis|deform|cyamid|lice|lethargic|lesion|discolor|diatom|
                      poor cond|poor health|poor body|poor over|poor skin|rake|skin|slough|thin |emaciated|malnourish|underweight|
                      starv|mobility", x$Narrative, ignore.case=TRUE)
     decline = as.numeric(lapply(decline, as.numeric))

# 'Extensive or Severe' case resulting from entanglement or vessel strike?

     extensive.severe = grepl("extensive|severe|substantial|massive|amputat", x$Narrative, ignore.case=TRUE)
      extensive.severe = as.numeric(lapply(extensive.severe, as.numeric))

# Did injury involve fluke or peduncle area?

      fluke.peduncle = grepl("fluke|peduncle|tail", x$Narrative, ignore.case=TRUE)
      fluke.peduncle = as.numeric(lapply(fluke.peduncle, as.numeric))

#  Evidence that whale is now gear-free after initial sighting? Or is expected to shed loose gear?

     gear.free = grepl("gear free|shed|gear-free|no gear present", x$Narrative, ignore.case=TRUE)
      gear.free = as.numeric(lapply(gear.free, as.numeric))

# Injury involved head, rostrum, or mouth?

      head = grepl("head|baleen|mouth|rostrum|lips", x$Narrative, ignore.case=TRUE)
       head = as.numeric(lapply(head, as.numeric))

# Deep laceration?

      laceration.deep = grepl("deep.*laceration|laceration.*deep|laceration.*muscle|muscle.*laceration|laceration.*blubber|blubber.*laceration|
                              laceration.*artery|artery.*laceration|laceration.*arteri|arteri.*laceration|laceration.*massive|
                              massive.*laceration|laceration.*penetrat|penetrat.*laceration|laceration.*necrotic|necrotic.*laceration|
                              large.*laceration|laceration.*large|laceration.*propell|propell.*laceration",
                              x$Narrative, ignore.case=TRUE)

      laceration.deep = as.numeric(lapply(laceration.deep, as.numeric))

# Shallow laceration?

      laceration.shallow = grepl("shallow.*laceration|laceration.*shallow|minor.*laceration|laceration.*minor|
                                 superficial.*laceration|laceration.*superficial|heal.*laceration|laceration.*heal|
                                 small.*laceration|laceration.*small", x$Narrative, ignore.case=TRUE)

      laceration.shallow = as.numeric(lapply(laceration.shallow, as.numeric))

# Evidence whale is / was healing / recovering?

      healing = grepl("healing|healed|healthy.*resight|resight.*healthy|good health", x$Narrative, ignore.case=TRUE)
       healing = as.numeric(lapply(healing, as.numeric))

# Did injury involve flipper/pectoral?

       pectoral = grepl("pectoral|flipper|pecs", x$Narrative, ignore.case=TRUE)
        pectoral = as.numeric(lapply(pectoral, as.numeric))

# Narrative includes reference to swimming and / or diving whale?
       swim.dive = grepl("free.*swimming|swimming.*free|swimming.*diving|diving.*swimming|swimming.*dove|dove.*swim", x$Narrative, ignore.case=TRUE)
       swim.dive = as.numeric(lapply(swim.dive, as.numeric))
# Was whale trailing gear?
       trailing = grepl("trail|towing|drag", x$Narrative, ignore.case=TRUE)
        trailing = as.numeric(lapply(trailing, as.numeric))

# Whale has wraps of gear (none or multiple?)

        wraps.no = grepl("no wrap", x$Narrative, ignore.case=TRUE)
        wraps.no = as.numeric(lapply(wraps.no, as.numeric))

        wraps.multi = grepl("multiple wraps|several wraps", x$Narrative, ignore.case=TRUE)
        wraps.multi = as.numeric(lapply(wraps.multi, as.numeric))

        x <- cbind.data.frame(x, anchored, calf.juv, constricting, decline, extensive.severe, fluke.peduncle, gear.free, head, healing,
                              laceration.deep, laceration.shallow, pectoral, swim.dive, trailing, wraps.multi, wraps.no)

# Begin vessel strike covariate section, append covariates to current data frame

# Vessel Variables (VSize and VSpeed)
#  Archive Vessel Size and Speed strings

# identify character vector that includes all unique numeric values in data$Narrative
       all.values <- unlist(regmatches(x$Narrative, gregexpr("[0-9]*\\.?[0-9]", x$Narrative)))
       all.values <- as.character(unique(as.numeric(all.values)))
       all.values <- as.numeric(sort(all.values))

# parse by vessel size (small, large) and speed (slow, fast) #####
# omit VSm values==5 due to grep identifying '65' (vessel size threshold) as a longer case of '5'
# there are no 5 ft long vessels other than kayaks and rafts, which are typically identified as such in narratives

       VSm <- all.values[all.values<65 & all.values!=5]
       VLg <- all.values[all.values>=65]

# Identify data frame records identified as vessel strike cases
       VData <- x[x$CAUSE=="VS",]
       VStrike.ind <- which(x$CAUSE=="VS")
       NotVStrike.ind <- seq(1,nrow(x))[-VStrike.ind]

# create character vectors of Vsize and VSpeed for use with grep, grepl, or regexpr

       VSzUnk.strings <- c("unable to determine vessel size|vessel size unknown|
                          unknown vessel size|size of vessel undetermined|vessel size
                          and speed unknown")

       VSm1 <- paste(" ", VSm, "ft", sep="", collapse="|")
       VSm2 <- paste(" ", VSm, " ft", sep="", collapse="|")
       VSm3 <- paste(" ", VSm, " feet", sep="", collapse="|")
       VSm4 <- paste(" ", VSm, " foot", sep="", collapse="|")
       VSm5 <- paste(" ", VSm, "'", sep="", collapse="|")

       VSm.strings <- paste(c(VSm1, VSm2, VSm3, VSm4, VSm5), sep=",", collapse="|")
       More.VSm.strings <- c("<65","-65", "->65", "less than 65", "sailboat",
                             "recreation", "rec boat", "kayak", "pleasure", "private", "sport")

       VSm.strings <- paste(c(VSm.strings, More.VSm.strings), sep=",", collapse="|")

       VLg1 <- paste(" ", VLg, "ft", sep="", collapse="|")
       VLg2 <- paste(" ", VLg, " ft", sep="", collapse="|")
       VLg3 <- paste(" ", VLg, " feet", sep="", collapse="|")
       VLg4 <- paste(" ", VLg, " foot", sep="", collapse="|")
       VLg5 <- paste(" ", VLg, "'", sep="", collapse="|")

       VLg.strings <- paste(c(VLg1, VLg2, VLg3, VLg4, VLg5), sep=",", collapse="|")

       More.VLg.strings <- c(" >65", "container ship", "size much greater than whale",
                             "in excess of 65 ft", "cruise ship", "navy", "transport",
                             "ferry", "express", "vessel larger than whale", "assumed to be larger",
                             "much larger than the whale", "wrapped around bow", "bow of a large ship",
                             "larger and faster than whale", "wrapped around bow")

       VLg.strings <- paste(c(VLg.strings, More.VLg.strings), sep=",", collapse="|")

       VSzUnk.ind <- grep(VSzUnk.strings, VData$Narrative, ignore.case=TRUE)
       VSm.ind <- grep(VSm.strings, VData$Narrative, ignore.case=TRUE)
       VLg.ind <- grep(VLg.strings, VData$Narrative, ignore.case=TRUE)

# Vessel Speed descriptions / definitions
# VSpeed threshold is <= 10 kts and >10 kts

       VSlow <- all.values[all.values<=10]
       VFast <- all.values[all.values>10]

       VSlow1 <- paste(" ", VSlow, "kt", sep="", collapse="|")
       VSlow2 <- paste(" ", VSlow, " kt", sep="", collapse="|")
       VSlow3 <- paste(" ", VSlow, "knots", sep="", collapse="|")
       VSlow4 <- paste(" ", VSlow, " knots", sep="", collapse="|")

       VFast1 <- paste(" ", VFast, "kt", sep="", collapse="|")
       VFast2 <- paste(" ", VFast, " kt", sep="", collapse="|")
       VFast3 <- paste(" ", VFast, "knots", sep="", collapse="|")
       VFast4 <- paste(" ", VFast, " knots", sep="", collapse="|")
       VFast5 <- paste(VFast, " knots", sep="", collapse="|")

       VSpdUnk.strings <- c("speed unknown|unknown speed|no data on vessel size and speed|
            unknown vessel size and speed|vessel size and speed unknown|unknown size and speed|
              speed of vessel unknown")

       VSlow.strings <- paste(c(VSlow1, VSlow2, VSlow3, VSlow4), sep=",", collapse="|")
       More.VSlow.strings <- paste("<10kt", "<10 kt", "<=10 kt", "<10 knots", sep="", collapse="|")
       VSlow.strings <- paste(c(VSlow.strings, More.VSlow.strings), sep=",", collapse="|")

       VFast.strings <- paste(c(VFast1, VFast2, VFast3, VFast4, VFast5), sep=",", collapse="|")
       More.VFast.strings <- c("wrapped around bow", "stuck on bow", "larger and faster than whale", "bow of a large ship", ">10kt", ">10 kt", ">10 knots")
       VFast.strings <- paste(c(VFast.strings, More.VFast.strings), sep=",", collapse="|")

       VSpdUnk.ind <- grep(VSpdUnk.strings, VData$Narrative, ignore.case=TRUE)
       VSlow.ind <- grep(VSlow.strings, VData$Narrative, ignore.case=TRUE)
       VFast.ind <- grep(VFast.strings, VData$Narrative, ignore.case=TRUE)

       VessSz <- rep(NA, nrow(VData))
       VessSpd <- rep(NA, nrow(VData))

       VessSz[VSzUnk.ind] <- "VSzUnk"
       VessSpd[VSpdUnk.ind] <- "VSpdUnk"

       VessSz[VSm.ind] <- "VSzSmall"
       VessSpd[VSlow.ind] <- "VSpdSlow"

       VessSz[VLg.ind] <- "VSzLarge"
       VessSpd[VFast.ind] <- "VSpdFast"

       VessSz[which(is.na(VessSz==TRUE))] <- "VSzUnk"
       VessSpd[which(is.na(VessSpd==TRUE))] <- "VSpdUnk"

       VData <- cbind.data.frame(VData, VessSz, VessSpd)
       NotVData <- x[NotVStrike.ind,]

# narratives in conflict for size and speed (addressed by assigning more severe speed / size
# category in 'Narrative'. Example: cases where size and speed of vessel are described as
# 'unknown', but the cruising speed or damage to whale allows inference of a large + fast
# vessel. Whale carcasses that come into port on the bow of a large container ship are assigned
# fast vessel speeds by default. Same for vessel size, if described as larger than whale, the
# vessel size is assigned as VSizeLg). To resolve these conflicts, the unknown categories are
# indexed first and narratives that contain matches between unknown and unknown categories are
# assigned the known category last (Fast supercedes Unknown and Slow, Large supercedes Unknown and Small)

       Small.In.Large.conflict <- stats::na.omit(match(VSm.ind, VLg.ind))
       Slow.In.Fast.conflict <- stats::na.omit(match(VSlow.ind, VFast.ind))

       ConflictVSize <- VData[VLg.ind[Small.In.Large.conflict],]
       ConflictVSpd <- VData[VFast.ind[Slow.In.Fast.conflict],]

       # assign unknown VessSz, VessSpd for non-vessel strike data
       VessSz <- rep("VSzUnk", nrow(NotVData))
       VessSpd <- rep("VSpdUnk", nrow(NotVData))
       NotVData <- cbind.data.frame(NotVData, VessSz, VessSpd)

       df <- rbind.data.frame(VData, NotVData)
       df
   }

#   df = cbind.data.frame(x, anchored, calf.juv, constricting, decline, extensive.severe, fluke.peduncle, gear.free, head, healing,
#   laceration.deep, laceration.shallow, pectoral, swim.dive, trailing, wraps.multi, wraps.no)


