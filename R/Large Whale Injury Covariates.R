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
#' @examples
#' head(LargeWhaleData)
#'
#' new.data.frame <- WhaleInjuryCovariates(LargeWhaleData)
#'
#' head(new.data.frame)
#'
#' @author Jim Carretta <jim.carretta@noaa.gov>
#'
#' @export

   WhaleInjuryCovariates = function(x) {

# 7-18-2021 3:48 pm
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
   anchored = grepl("anchor|motionless|unable to move|stationary|entrap|weighted", x$Narrative, ignore.case=T)
    anchored = as.numeric(lapply(anchored, as.numeric))

# Was a calf, juvenile or lactating mother involved in the case?
    calf.juv = grepl("calf|juvenile|young|dependent", x$Narrative, ignore.case=T)
     calf.juv = as.numeric(lapply(calf.juv, as.numeric))

# Evidence of constricting entanglement?
     constricting = grepl("constricting|tight|cutting|impress|embed|pinn|twisted|necrotic|amputat|missing.*fluke|fluke.*missing", x$Narrative, ignore.case=T)
      constricting = as.numeric(lapply(constricting, as.numeric))

# Evidence of a health decline?
    decline = grepl(" abnormal|chronic|deteriorat|fair|compromise|scoliosis|deform|cyamid|lice|lethargic|lesion|discolor|diatom|
                      poor cond|poor health|poor body|poor over|poor skin|rake|skin|slough|thin |emaciated|malnourish|underweight|
                      starv|mobility", x$Narrative, ignore.case=T)
     decline = as.numeric(lapply(decline, as.numeric))

# 'Extensive or Severe' case resulting from entanglement or vessel strike?

     extensive.severe = grepl("extensive|severe|substantial|massive|amputat", x$Narrative, ignore.case=T)
      extensive.severe = as.numeric(lapply(extensive.severe, as.numeric))

# Did injury involve fluke or peduncle area?

      fluke.peduncle = grepl("fluke|peduncle|tail", x$Narrative, ignore.case=T)
      fluke.peduncle = as.numeric(lapply(fluke.peduncle, as.numeric))

#  Evidence that whale is now gear-free after initial sighting? Or is expected to shed loose gear?

     gear.free = grepl("gear free|shed|gear-free|no gear present", x$Narrative, ignore.case=T)
      gear.free = as.numeric(lapply(gear.free, as.numeric))

# Injury involved head, rostrum, or mouth?

      head = grepl("head|baleen|mouth|rostrum|lips", x$Narrative, ignore.case=T)
       head = as.numeric(lapply(head, as.numeric))

# Deep laceration?

      laceration.deep = grepl("deep.*laceration|laceration.*deep|laceration.*muscle|muscle.*laceration|laceration.*blubber|blubber.*laceration|
                              laceration.*artery|artery.*laceration|laceration.*arteri|arteri.*laceration|laceration.*massive|
                              massive.*laceration|laceration.*penetrat|penetrat.*laceration|laceration.*necrotic|necrotic.*laceration|
                              large.*laceration|laceration.*large|laceration.*propell|propell.*laceration",
                              x$Narrative, ignore.case=T)

      laceration.deep = as.numeric(lapply(laceration.deep, as.numeric))

# Shallow laceration?

      laceration.shallow = grepl("shallow.*laceration|laceration.*shallow|minor.*laceration|laceration.*minor|
                                 superficial.*laceration|laceration.*superficial|heal.*laceration|laceration.*heal|
                                 small.*laceration|laceration.*small", x$Narrative, ignore.case=T)

      laceration.shallow = as.numeric(lapply(laceration.shallow, as.numeric))

# Evidence whale is / was healing / recovering?

      healing = grepl("healing|healed|healthy.*resight|resight.*healthy|good health", x$Narrative, ignore.case=T)
       healing = as.numeric(lapply(healing, as.numeric))

# Did injury involve flipper/pectoral?

       pectoral = grepl("pectoral|flipper|pecs", x$Narrative, ignore.case=T)
        pectoral = as.numeric(lapply(pectoral, as.numeric))

# Narrative includes reference to swimming and / or diving whale?
       swim.dive = grepl("free.*swimming|swimming.*free|swimming.*diving|diving.*swimming|swimming.*dove|dove.*swim", x$Narrative, ignore.case=T)
       swim.dive = as.numeric(lapply(swim.dive, as.numeric))
# Was whale trailing gear?
       trailing = grepl("trail|towing|drag", x$Narrative, ignore.case=T)
        trailing = as.numeric(lapply(trailing, as.numeric))
# vessel size (small or large?)
       vessel.small = grepl("sailboat|sport|pleasure|zodiac|rhib|catamaran|recreat|rec boat|fishing|
                            boat|smaller|engine damage|snap|vessel.<|<.*65", x$Narrative, ignore.case=T)
        vessel.small = as.numeric(lapply(vessel.small, as.numeric))
         vessel.large = grepl("ship|RV|dredge|navy|naval|military|ferry|carrier|cutter|yacht|larger|whale watch|WW|>.*65", x$Narrative, ignore.case=T)
          vessel.large = as.numeric(lapply(vessel.large, as.numeric))
# vessel speed (slow / fast? threshold used in SI protocols is 10 kts)
# identify x$Narrative phrases / characters that identify slow and fast (<=>10 kts) vessels, respectively.
        vessel.slow.list1 = c(paste(seq(0.1,10,0.1), "kts", sep=" "))
        vessel.slow.list2 = c(paste(seq(0.1,10,0.1), "kts", sep=""))
        vessel.slow.list = c(vessel.slow.list1, vessel.slow.list2)
        vessel.slow.list = paste("", vessel.slow.list)
        vessel.slow.list = c(paste(vessel.slow.list, collapse="|"))
        other.vessel.slow = c("|<10kts|< 10kts|<10 kts|< 10 kts")
        vessel.slow.list = paste(vessel.slow.list, other.vessel.slow, sep="")
        vessel.slow = grepl(vessel.slow.list, x$Narrative, ignore.case=T)
        vessel.slow = as.numeric(lapply(vessel.slow, as.numeric))

        vessel.fast.list1 = c(paste(seq(10.1,45,0.1), "kts", sep=" "))
        vessel.fast.list2 = c(paste(seq(10.1,45,0.1), "kts", sep=""))
        vessel.fast.list = c(vessel.fast.list1, vessel.fast.list2)
        vessel.fast.list = paste("", vessel.fast.list)
        vessel.fast.list = c(paste(vessel.fast.list, collapse="|"))
        other.vessel.fast = c("|>10kts|> 10kts|>10 kts|> 10 kts")
        vessel.fast.list = paste(vessel.fast.list, other.vessel.fast, sep="")

        vessel.fast = grepl(vessel.fast.list, x$Narrative, ignore.case=T)
        vessel.fast = as.numeric(lapply(vessel.fast, as.numeric))

# Whale has wraps of gear (none or multiple?)

        wraps.no = grepl("no wrap", x$Narrative, ignore.case=T)
        wraps.no = as.numeric(lapply(wraps.no, as.numeric))

        wraps.multi = grepl("multiple wraps|several wraps", x$Narrative, ignore.case=T)
        wraps.multi = as.numeric(lapply(wraps.multi, as.numeric))

# Append covariates to data frame (x) and return appended data frame object

     df = cbind.data.frame(x, anchored, calf.juv, constricting, decline, extensive.severe, fluke.peduncle, gear.free, head, healing, laceration.deep,
                                   laceration.shallow, pectoral, swim.dive, trailing, vessel.slow, vessel.fast, vessel.small, vessel.large, wraps.multi, wraps.no)

     df

     }

