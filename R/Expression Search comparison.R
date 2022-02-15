#### Code chunk to test for grepl searches when line ends with "|" vs not....
#### Ending line with "|" results in 'FALSE' vs 'TRUE' for 'Small laceration' search


rm(list=ls())

Narrative = c("event added 2014. WW Voyager III hit a whale. Vessel drifting/slow-ahead.
                Animal came up under starboard hull, rocking it slightly. Small laceration
                forward of dorsal. Vessel Speed < 10 kts. Vessel Size > 65 ft.")

x <- as.data.frame(Narrative)


grepl("shallow.*laceration|laceration.*shallow|minor.*laceration|laceration.*minor
 |superficial.*laceration|laceration.*superficial|heal.*laceration|laceration.*heal
  |small.*laceration|laceration.*small", x$Narrative, ignore.case=TRUE)

grepl("shallow.*laceration|laceration.*shallow|minor.*laceration|laceration.*minor|
 superficial.*laceration|laceration.*superficial|heal.*laceration|laceration.*heal|
  small.*laceration|laceration.*small", x$Narrative, ignore.case=TRUE)
